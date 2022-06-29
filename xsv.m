classdef xsv < handle
% TODO, multiple tables in a file
% XXX <> is file handle
% XXX > is an object
% TODO rm cols
% multiple names into cell
properties
    fname
    OUT=struct;
    lines
end
properties(Hidden=true)
    ZER
    F
    header

    parents

    parentRow
    parentCol
    nameCol

    linum
    spec_str
    err_char

    iInd
    bInd
    fInd
    nInd
    emptyInd
    parentInd
    parentNames={'parent','Parent','PARENT'};
    nameNames={'name','NAME','Name'};

    intPrefix='i_'
    binPrefix='b_'
    funPrefix='f_'
    numPrefix='n_'


end
methods
    function obj=xsv(fname,cha,spec_str,err_char)
        if ~exist('cha','var') || isempty(cha)
            obj.F='|';
        end
        if exist('spec_str','var')
            obj.spec_str=spec_str;
        end
        if ~eixst('err_char','var')
            obj.err_char=err_char;
        end
        obj.fname=fname;

        obj.get_lines();
        obj.get_header();
        obj.rm_sep();
        obj.split_at_F();
        obj.expand_lines();
        if ~isemtpy(obj.err_char)
            obj.check_for_err_char();
        end
        if ~isempty(obj.spec_str)
            obj.rm_special_str();
        end
    end
    function obj=parse(obj)
        obj.get_inds();
        obj.fill_in_parent();

        obj.check_binary(); % TEST
        obj.check_numeric(); % TEST

        obj.fill_in_from_parent();
        obj.emptyInd=cellfun(@isempty,obj.lines);

        obj.evaluate_statements();
        obj.fill_in_binary();
        obj.convert_numeric();
        obj.fill_in_empty();
        obj.convert_func();
        obj.convert_select_val(); % TEST

        obj.convert_default();

        obj.run_checks(); % TEST

        obj.rm_parent_rows();

        obj.populate();
    end
end
methods (Hidden=true)
    function obj=convert(fname,cha)
    end
    function obj=check_for_err_char(obj)
        [qRow,qCol]=find(cellfun(@(x) contains(x,obj.err_char), obj.lines));
        if ~isempty(qRow)
            r=cellstr(strcat(obj.linum(qRow,:),', '));
            [r,ind]=sort(r);
            r=[r{:}];
            r=r(1:end-1);

            c=cellstr(strcat(obj.linum(qCol,:),', '));
            c=c(ind);
            c=[c{:}];
            c=c(1:end-1);
            error(['err_char ' obj.err_char ' found in meta-param file at row(s) ' r ' and column(s) ' c '.'])
        end
    end
    function obj=rm_special_str(obj)
        ind=cellfun(@(x) spec_str_fun(obj.spec_str,x),obj.lines);
        obj.lines(ind)={[]};
        obj.lines=cellfun(@rep_fun,obj.lines,UniformOutput,false);
        function out=rep_fun(in)
            if iscell(in) || ischar(in)
                out=strrep(in,'?','');
            else
                out=in;
            end
        end
        function out=spec_str_fun(spec_char,x)
            out=any(cellfun(@(y) strcmp(x,y),x));
        end
    end
end
methods(private=true)
    function obj=get_lines(obj)
        obj.lines=file2cell(obj.fname);
        obj.linum=num2str((1:length(obj.lines))');
    end
    function obj=get_header(obj)
        for i = 1:length(obj.lines)
            if ~isempty(obj.lines{i}) && all(ismember(unique(obj.lines{i}),'|-+'))
                break
            end
        end
        obj.header=obj.lines{i-1};
        obj.lines(i-1:i)=[];
        obj.linum(i-1:i,:)=[];
    end
    function obj=rm_sep(obj)
        ind=cellfun(@(x) isempty(x) | all(ismember(unique(x),'|-+')),obj.lines);
        obj.lines(ind)=[];
        obj.linum(ind,:)=[];
    end
    function obj=split_at_F(obj)
        obj.header=strsplit(obj.header,obj.F);
        ind=cellfun(@isempty,obj.header);
        obj.header(ind)=[];
        obj.header=strtrim(obj.header);
        for i = 1:length(obj.lines)
            obj.lines{i}=strsplit(obj.lines{i},obj.F);
            obj.lines{i}(ind)=[];
            obj.lines{i}=strtrim(obj.lines{i});
        end
    end
    function obj=expand_lines(obj)
        obj.lines=vertcat(obj.lines{:});
    end
    function obj=get_inds(obj)
        obj.ZER=zeros(size(obj.lines));
        obj.get_parent_inds();
        obj.get_key_inds();
        obj.get_type_inds();
        obj.emptyInd=cellfun(@isempty,obj.lines);
    end
    function obj=get_parent_inds(obj)
        m=size(obj.lines,2);

        obj.parentCol=find(cellfun(@(x) ismember(x,obj.parentNames),obj.header));
        obj.nameCol  =find(cellfun(@(x) ismember(x,obj.nameNames),obj.header));

        obj.parentRow=cellfun(@(x) ~isempty(x),obj.lines(:,obj.parentCol));
        obj.parentInd=repmat(obj.parentRow,1,m);
    end
    function obj=get_type_inds(obj)
        n=size(obj.lines,1);
        obj.iInd=repmat(startsWith(obj.header,obj.intPrefix),n,1);
        obj.bInd=repmat(startsWith(obj.header,obj.binPrefix),n,1);
        obj.fInd=repmat(startsWith(obj.header,obj.funPrefix),n,1);
        obj.nInd=repmat(startsWith(obj.header,obj.numPrefix),n,1);
    end
    function obj=fill_in_parent(obj)
        obj.parents=obj.lines(obj.parentRow,obj.parentCol);
        inds=Vec.row(cumsum(obj.parentRow));
        for i=unique(inds)
            obj.lines(inds==i,obj.parentCol)={obj.parents{i}};
        end
    end
    function obj=check_binary(obj);
        if any(~(obj.emptyInd(obj.bInd) | ismember(obj.lines(obj.bInd),{'1','0'})))
            error('Invalid binary value in meta-param file.')
        end
        % TODO specific locations
    end
    function obj=check_numeric(obj);
        if any(~(obj.emptyInd(obj.bInd) | ismember(obj.lines(obj.bInd),numV)))
            error('Invalid numeric value in meta-param file.')
        end
        % TODO specific locations
    end
    function obj=fill_in_from_parent(obj)
        ind=obj.parentInd & ~obj.emptyInd;
        ind(:,obj.parentCol)=false;
        inds=Vec.col(cumsum(obj.parentRow));
        m=size(ind,2);

        for ii=unique(inds)'
            i=(inds==ii);
            I=repmat(i,1,m);

            getInd=ind & I;
            cols=find(any(getInd));
            fillInd=obj.ZER;
            fillInd(:,cols)=1;
            fillInd=fillInd & obj.emptyInd & I;
            vals=Vec.row(obj.lines(getInd));

            for j = 1:length(vals)
                obj.lines(fillInd(:,cols(j)),cols(j))={vals{j}};
            end

        end
    end
    function obj=fill_in_binary(obj)
        obj.lines(obj.emptyInd & obj.bInd)={'0'};
        obj.lines(obj.bInd)=num2cell(cellfun( @(x) x=='1', obj.lines(obj.bInd)));
        obj.emptyInd=cellfun(@isempty,obj.lines);
    end
    function obj=evaluate_statements(obj)
        stateInd=cellfun(@(x) any(ismember(x,',')),obj.lines) & (obj.bInd | obj.nInd | obj.iInd);
        obj.lines(stateInd)=cellfun(@(x) eval(['[' x ']']) ,obj.lines(stateInd),UniformOutput,false);

        stateInd=cellfun(@(x) ischar(x) && contains(x,{',',':','/','*'}),obj.lines) & (obj.bInd | obj.nInd | obj.iInd);
        obj.lines(stateInd)=cellfun(@eval, obj.lines(stateInd),UniformOutput,false);

    end
    function obj=convert_numeric(obj)
        ind=cellfun(@ischar,obj.lines) & ~obj.emptyInd & (obj.nInd | obj.iInd);
        obj.lines(ind)=num2cell(cellfun(@str2double,obj.lines(ind)));
    end
    function obj=convert_func(obj)
        obj.lines(:,obj.testCol)=cellfun(@convert_fun , obj.lines(:,obj.testCol),UniformOutput,false);
        function out=convert_fun(x)
            if isfunction(x)
                out=str2func(x);
            elseif startswith(x,'#')
                out=x;
            else
                error(['function ''' x ''' does not exit']);
            end
        end
    end
    function obj=convert_select_val(obj);
        bCell=cellfun(@(x) startsWith(x,'{') & endsWith(x,'}'),obj.lines(:,obj.valCol));
        bNum =cellfun(@(x) startsWith(x,'[') & endsWith(x,']'),obj.lines(:,obj.valCol));
        obj.lines(bNum,obj.valCol)=num2cell(cellfun(@str2double,obj.lines(bNum,obj.valCol)));
        obj.lines(bCell,obj.valCol)=cellfun(@eval,obj.lines(bCell,obj.valCol),UniformOutput,false);
    end
    function obj=fill_in_empty(obj)
        obj.lines(obj.emptyInd & (obj.defInd | obj.iInd | obj.nInd))={[]};
        obj.lines(obj.emptyInd & obj.fInd)={'true'};
    end
    function obj=rm_parent_rows(obj)
        obj.lines(obj.parentRow,:)=[];
    end
    function obj=populate(obj)
        if isprop(obj,'bRotate')
            flag=1;
        end
        bVal=~cellfun(@isempty,obj.lines(:,obj.valCol));

        M=1:size(obj.lines,2);
        M(M==obj.parentCol | M==obj.nameCol)=[];
        for i = 1:length(obj.parents)
            fld=obj.parents{i};
            obj.OUT.(fld)=struct();
        end
        for i = 1:size(obj.lines,1)
            line=obj.lines(i,:);
            par=line{obj.parentCol};
            name=line{obj.nameCol};
            obj.OUT.(par).(name)=struct();
            for m=M
                fld=obj.header{m};
                val=line{m};
                obj.OUT.(par).(name).(fld)=val;
            end
            if flag
                obj.OUT.(par).(name).bRotate=bVal(i);
            end
            obj.OUT.(par).(name).VALUE=[];
        end
    end
    function obj=get_key_inds(obj)
        % for subclasses
    end
    function obj=convert_defaults(obj)
        % for subclasses
    end
    function obj=run_checks(obj);
        % for subclasses
    end
end
methods(Static=true)
    function OUT= read(fname,cha)
        obj=xsv(fname,cha);
        OUT=obj.OUT;
    end
    function OUT=read_parse(fname,cha)
        obj=xsv(fname,cha)
        obj.parse(fname,cha)
        OUT=obj.OUT;
    end
end
end
