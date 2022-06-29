classdef Table < handle & matlab.mixin.CustomDisplay
properties
end
%properties(Access=private)
properties(Hidden)
    TABLE
    KEY
    types
    K
end
properties(Access=private,Constant)
    numTypes={'double','logical','single','uint8','int8','uint16','int16','uint32','int32','uint64','int64'};
    PROPS={'TABLE','KEY','types','K','Meths'};
end
methods
    function obj=Table(table,key, types)
        if nargin < 1
            return
        end
        if ~isempty(table) && numel(key)~=size(table,2);
            error('Key and table dimensions do not match');
        end
        obj.KEY=key;
        if ~iscell(table)
            table=num2cell(table); % SLOWISH
        end

        M=size(obj.KEY,2);
        if nargin >= 3 && ~isempty(types)
            obj.types=types;
        else
            obj.types=cell(1,M);
            for m = 1:M
                obj.types(1,m)=unique(cellfun(@class, table(:,m),'UniformOutput',false)); %  SLOW
            end
        end

        obj.TABLE=cell(1,M);
        if isempty(table)
            for m = 1:M
                if ismember_cell(obj.types{1,m}, Table.numTypes);
                    obj.TABLE{m}=[];
                else
                    obj.TABLE{m}=cell(0,1);
                end
            end
            return
        end
        for m = 1:M
            if numel(obj.types{1,m})
                if strcmp(obj.types{1,m},'cell')
                    obj.TABLE{m}=table(:,m);
                elseif ismember_cell(obj.types{1,m}, Table.numTypes);
                    try
                        obj.TABLE{m}=cell2mat(table(:,m));  % SLOW
                    catch ME
                        table(:,m)
                        rethrow(ME)
                    end
                elseif strcmp(obj.types{1,m},'char')
                    if iscell(table{1,m})
                        obj.TABLE{m}=vertcat(table{:,m});
                        %obj.TABLE{m}=table{:,m};
                    else
                        obj.TABLE{m}=table(:,m);
                    end
                end
            end
        end
    end
%% STAT
    function out=length(obj)
        out=obj.size(1);
    end
    function out=width(obj)
        out=obj.size(2);
    end
    function sz=size(obj,dim)
        if isempty(obj.TABLE)
            sz=[0 0];
        else
            sz=[size(obj.TABLE{1},1) numel(obj.TABLE)];
        end
        if exist('dim','var') && ~isempty(dim)
            sz=sz(dim);
        end
    end
%% SUBS
    function T=each(obj,varargin)
        K=subs_parse(obj,varargin);

        bSingle=length(K.vals) > 1;
        if bSingle;
            vals=Set.distribute(K.vals{:});
        else
            vals=K.vals{1};
        end
        [uvals,~,uind]=unique(vals,'rows');
        uvals={uvals};

        KU=K;
        KU.vals=uvals;
        T=subsref_ind(obj,[],KU);
        T.TABLE=cellfun(@(x) x(uind) ,T.TABLE,'UniformOutput',false);
    end
    function t=each_old(obj,varargin)
        [t,K]=subsref_ind(obj,varargin);

        if length(K.vals) > 1
            vals=Set.distribute(K.vals{:});
        else
            vals=K.vals;
        end
        K.idx=1:length(t);
        T=Table();
        for i = 1:size(vals,1)
            k=K;
            k.vals=num2cell(vals(i,:));
            t=subsref_ind(t,[],k);
            T.TABLE=[T.TABLE; t.TABLE];
        end
        T.KEY=t.KEY;
        t=T;
    end
    function n = numArgumentsFromSubscript(obj,s,indexingContext)
        % CELL-LIKE OUTPUT
        if strcmp(s(1).type,'{}')
            if strcmp(s(1).subs{1},':')
                n=width(obj);
            elseif ~isempty(obj.K)
                if ~isempty(obj.K.outKeys)
                    n=length(obj.K.outKeys);
                else
                    n=length(obj.KEY);
                end
            else
                n=sum(cellfun(@ischar,s(1).subs));
                if strcmp(s(1).subs(end),':')
                    n=n-1;
                end
            end
        elseif indexingContext == matlab.mixin.util.IndexingContext.Expression
            n = 1;
        elseif iscell(s(1).subs)
            n = length(s(1).subs{:});
        else
            n = 1;
        end
    end
    function [varargout]=subsref(obj,s)
        switch s(1).type
        case '.'
            %if strcmp(s(1).subs,'TABLE') || strcmp(s(1).subs,'KEY') || strcmp(s(1).subs,'types') || strcmp(s(1).subs,'types')

            %    n=nargout;
            %else
            %    n=nargout(['Table>Table.' s(1).subs]);
            %    if n==-1
            %        n=nargout;
            %    end
            %end
            %[varargout{1:n}] = builtin('subsref',obj,s);
            [varargout{1:nargout}] = builtin('subsref',obj,s);
        case '()'
            if ischar(s(1).subs{1}) && strcmp(s(1).subs{1},':')
                varargout{1}=obj;
            else
                out=subsref_ind(obj,[s(1).subs]);
                if length(s) > 1
                    [varargout{1:nargout}]=subsref(out,s(2:end));
                else
                    varargout{1}=out;
                end
            end
        case '{}'
            if length(s(1).subs)==1 && ischar(s(1).subs{1}) && strcmp(s(1).subs{1},':')
                % T{:}
                out=obj;
            else
                out=subsref_ind(obj,s(1).subs);
            end

            if length(s) > 1
                if strcmp(s(end).subs,':')
                % T{...}{:}
                    if length(s) > 3
                        [varargout{1:nargout}]=subsref(out,s(2:end-1));
                    else
                        varargout=out.TABLE;
                    end
                    varargout=cellfun(@unnest_fun,varargout,'UniformOutput',false);
                else
                    [varargout{1:nargout}]=subsref(out,s(2:end));
                end
            else
                %varargout{1}=out;
                varargout=out.TABLE;
            end
        end
        function out=unnest_fun(x)
            if iscell(x)
                out=x{:};
            else
                out=x;
            end

        end
    end
    function [t,K]=subsref_ind(obj,subs, K)
        if nargin < 3 || isempty(K)
            K=subs_parse(obj,subs);
        end
        IND=subs_condition(obj,K); % SLOW
        if isempty(K.outKeys)
            limit=true(1,length(obj.KEY));
        else
            limit=ismember_cell(obj.KEY,K.outKeys);
        end
        t=Table();
        t.KEY=obj.KEY(limit);
        t.types=obj.types(limit);
        t.TABLE=cellfun(@(x) [x(IND,:)] ,obj.TABLE(limit),'UniformOutput',false);

        % ORDER ROWS ACCORDING TO INDEX
        if ~K.bUniform
            [~,~,idx]=unique(K.idx);
            t.TABLE=cellfun(@(x) [x(idx,:)] , t.TABLE,'UniformOutput',false);
        end

        % ORDER COLS ACCORDING TO OUT ORDER
        %[~,ind]=ismember(t.KEY,K.outKeys);
        if ~isempty(K.outKeys)
            [~,ind]=ismember_cell(K.outKeys,t.KEY);
            t.KEY=t.KEY(ind);
            t.TABLE=t.TABLE(ind);
            t.types=t.types(ind);
        end
        obj.K=[];
    end
    function obj=subsasgn(obj,S,val)
        switch S.type
            case '.'
                obj=builtin('subsasgn',obj,S,val);
                return
        end
        K=subs_parse(obj,S.subs);
        IND=subs_condition(obj,K);

        if isempty(K.outKeys)
            K.outKeys=K.KEYS;
        end
        for i = 1:length(K.outKeys)
            ind=ismember_cell(obj.KEY,K.outKeys{i});
            if (strcmp(obj.types{ind},'char') || strcmp(obj.types{ind},'cell')) && ischar(val)
                obj.TABLE{ind}(IND)={val};
            else
                obj.TABLE{ind}(IND)=val;
            end
        end
    end
    function K=subs_parse(obj,S, KEYS)
        if nargin < 3 || isempty(KEYS)
            KEYS=obj.KEY;
        end
        K=struct();

        % IDX
        if isnumeric(S{1})
            idx=S{1};
            S(1)=[];
            if any(idx < 1)
                error('Index value less than 1');
            elseif any(idx > size(obj,1))
                error('Index value greater than length');
            end
            K.bUniform=all(diff(idx)==1);
        else
            idx=1:size(obj,1);
            K.bUniform=true;
        end

        %ASSIGN SIGNS AND INEQ
        kind=cellfun(@(x) ischar(x) && ismember_cell(x,KEYS),S);
        cind=cellfun(@(x) ischar(x) && strcmp(x,':'),S);
        ineqInd=cellfun(@(x) ischar(x) && ismember_cell(x,{'=','==','>','<','>=','<=','~='}) ,S);
        signInd=cellfun(@(x) ischar(x) && ismember_cell(x,{'&','|'}),S);
        valInd=~kind & ~ ineqInd & ~signInd;
        IND=cumsum(kind);

        groups=unique(IND); % SLOW
        groups(groups==0)=[];
        N=numel(groups);
        keys=cell(N,1);
        vals=cell(N,1);
        signs=cell(N,1);
        ineqs=cell(N,1);
        outKeys={};
        for i = 1:N
            ind=IND==i;
            n=sum(ind);
            if n == 1
                outKeys{end+1}=S{ind};
                continue
            end
            keys{i}=S{kind & ind};
            vals{i}=S{valInd & ind};

            sind=signInd & ind;
            if ~any(sind);
                signs{i}='&';
            else
                signs{i}=S{signInd & ind};
            end
            iind=ineqInd & ind;
            if ~any(iind)
                ineqs{i}='==';
            else
                ineqs{i}=S{ineqInd & ind};
            end
        end
        rmInd=cellfun(@isempty,keys);
        keys(rmInd)=[];
        vals(rmInd)=[];
        signs(rmInd)=[];
        ineqs(rmInd)=[];

        K.KEYS=KEYS;
        K.idx=idx;
        K.keys=keys;
        K.vals=vals;
        K.signs=signs;
        K.ineqs=ineqs;
        K.outKeys=outKeys;

        obj.K=K;
        % XXX TODO CHECK VALS WITH TYPE

    end
    function IND=subs_condition(obj,K)
        %- HERE
        IND=false(obj.length,1);
        IND(K.idx)=true;
        for i = 1:length(K.keys)
            m=ismember_cell(obj.KEY,K.keys{i});
            ineq=get_ineq(K.ineqs{i});

            str=[ 'IND = IND ' K.signs{i} ' ' ineq '(obj.TABLE{m}, K.vals{i});'];
            try
                eval(str);
            catch ME
                disp(str)
                rethrow(ME);
            end
        end

        %m=ismember_cell(obj.KEY,K.keys{i});
        %ind = ismember(obj.TABLE{m}, K.vals{i});
        %K.outKeys={'lvlInd'};
        %limit=ismember_cell(obj.KEY,K.outKeys);
        %t=cellfun(@(x) [x(ind,:)] ,obj.TABLE(limit),'UniformOutput',false);
        %t{:}
        %ineq=get_ineq(K.ineqs{i});
        %str=[ ''];
        %str
        %eval(str)
        %ind

        function ineq=get_ineq(in)
            switch in
                case {'==','='}
                    % TODO make sure dimensions match
                    % TODO use eq if 1 element and numeric
                    %if ismember(obj.types{m},{'cell','char'});
                        ineq='ismember';
                    %else
                    %    ineq='eq';
                    %end
                case '~='
                    %if ismember(obj.types{m},{'cell','char'});
                        ineq='~ismember';
                    %else
                    %    ineq='~eq';
                    %end
                case '>'
                    ineq='gt';
                case '<'
                    ineq='lt';
                case '>='
                    ineq='ge';
                case '<='
                    ineq='le';
            end
        end
    end
    %function out=first(obj,varargin)
    %    [t,K]=subsref_ind(varargin{:});
    %    t=t.unique_rows();
    %end
    function [C,ia,ic,counts]=unique(obj,varargin)
        if length(varargin) > 0
            o=subsref_ind(obj,varargin);
            t=o.TABLE;
        else
            t=obj.TABLE;
        end
        counts=[];

        % XXX better solution? char need to be stored as cell cell?
        ind=find(cellfun(@iscell,t));
        for i = 1:length(ind)
            if ismember_cell(obj.types(ind(i)),{'cell','char'})
                t{ind(i)}=vertcat(t{ind(i)});
            else
                t{ind(i)}=t{ind(i)}{1};
            end
        end

        [C,ia,ic]=cellfun(@unique_fun ,t,'UniformOutput',false);
        if numel(C)==1
            C=C{1};
            ia=ia{1};
            ic=ic{1};
            counts=transpose(hist(ic,unique(ic)));
        end
        function [C,ia,ic]=unique_fun(x)
            if iscell(x)
                [C,ia,ic]=unique(x);
            else
                [C,ia,ic]=unique(x,'rows');
            end
        end

    end
    function [t,ind,ic]=unique_rows(obj,varargin)
        if length(varargin) > 1
            s=struct('type','()','subs',[]);
            s.subs=varargin;
            t=obj.subsref(s);
        else
            t=Obj.copy(obj);
        end
        ic=cellfun(@(x,y) unique_fun(x,y),t.TABLE,t.types,'UniformOutput',false);
        ic=horzcat(ic{:});
        [~,ind,ic]=unique(ic,'rows');
        t=subsref_ind(t,{ind});

        function c=unique_fun(col,type)
            if ismember_cell(type,{'char','cell'})
                [~,~,c]=unique(col);
            else
                [~,~,c]=unique(col,'rows');
            end
        end
    end
    function ind=find(obj,varargin)
        subs=varargin;
        K=subs_parse(obj,subs);
        ind=find(subs_condition(obj,K));
    end
    function out=sort(obj,varargin)
        subs=varargin;
        K=subs_parse(obj,subs);
        %ind=obj.subs_condition(K);
        [~,ind]=ismember_cell(K.outKeys,obj.KEY);
        [~,ind]=sortrows([obj.TABLE{ind}]);
        if nargout > 0
            out=Obj.copy(obj);
            out.order(ind);
        else
            obj.order(ind);
        end
    end
    function obj=sort_columns(obj,type,bReverse)
        % XXX
    end
    function order(obj,inds)
        obj.TABLE=cellfun(@(x) x(inds),obj.TABLE,'UniformOutput',false);
    end
    function order_cols(obj,inds)
        obj.TABLE=obj.TABLE(inds);
        obj.KEY=obj.KEY(inds);
        obj.types=obj.types(inds);
    end
%% COMBINE
    function add_row(obj,varargin)
        keys=varargin(1:2:end);
        vals=varargin(2:2:end);
        tbl=obj.TABLE;
        for i = 1:length(obj.KEY)
            K=obj.KEY{i};
            ind=ismember_cell(keys,K);
            if any(ind)
                if iscell(tbl{ind})
                    tbl{i}{end+1,1}=vals{ind};
                else
                    tbl{i}(end+1,1)=vals{ind};
                end
            else
                if iscell(tbl{i})
                    tbl{i}{end+1,1}=nan;
                else
                    tbl{i}(end+1,1)=nan;
                end
            end
        end
        obj.TABLE=tbl;
    end
    function add_col(obj)
        % XXX
    end
    function out=plus(varargin)
        out=vertcat(varargin{:});
    end
    function out=vertcat(varargin)
        N=length(varargin);
        for i = 1:N
            KEYS{i}=varargin{i}.KEY;
            TABLES{i}=varargin{i}.TABLE;
            TYPES{i}=varargin{i}.types;
        end
        % TODO
        % order according to first
        % make sure types are fine


        TABLES=vertcat(TABLES{:});
        M=numel(KEYS{1});
        for j = 1:M
            TABLES{1,j}=vertcat(TABLES{:,j});
            %if strcmp(TYPES{1}),'char') || strcmp(TYPES{1}{j},'cell')
            %end
        end
        TABLES(2:end,:)=[];

        out=Table();
        out.KEY=KEYS{1};
        out.TABLE=TABLES;
        out.types=TYPES{1};
    end
    function horzcat(obj,T)
        % XXX
    end
%% MODIFY
    function rename_col()
        % XXX
    end
    function cast()
        % XXX
    end
%% IN
    function T=from_cell()
        % XXX
    end
    function T=from_cellstruct()
        % XXX
    end
%% OUT
    function varargout=ret(obj)
        if nargout == 1 || width(obj) == 1
            for i= 1:width(obj)
                out{i}=obj.TABLE{i};
            end
            if all(ismember_cell(obj.types,obj.numTypes))
                varargout{1}=horzcat(out{:});
            end
        else
            for i= 1:nargout
                varargout{i}=obj.TABLE{i};
            end
        end
    end
    function S=rowAsStruct(obj,n)
        vals=cell(numel(obj.KEY),1);
        s=substruct('()',num2cell(n));
        r=subsref(obj,s);

        [vals{:}]=r.ret();
        p=[obj.KEY' vals]';
        S=struct(p{:});

    end
    function S=struct(obj)
        S=struct();
        for i = 1:length(key)
            S.(key{i})=obj.Table(:,i);
        end
    end
    function out=string(obj)
        kW=cellfun(@length,obj.KEY);
        [tableStr,w,wo]=Table.cell2strTable(obj.TABLE,2,0,kW);
        keyStr=cell(length(obj.KEY),1);
        wo(wo < 1)=0;
        minSzs=max([kW; wo],[],1);
        for i = 1:length(obj.KEY)
            keyStr(i)=Table.space_fun(obj.KEY(i),3,minSzs(i));
        end

        keyStr=join(keyStr,'');
        keyStr=keyStr{1};
        div=[repmat('_',1,length(keyStr)-1) newline];
        out=[keyStr newline  div tableStr ''];
    end
%% SAVE
    function save(obj,fname)
        table=obj.TABLE;
        key=obj.KEY;
        types=obj.types;
        save(fname,'key','table','types');
    end
%% UTIL
    function out=isnotkey(obj,val)
        %TODO expand this
        out=~obj.iskey(val) && ~ismember_cell('char',obj.types);
    end
    function out=iskey(obj,val)
        if isnumeric(val)
            out=0;
            return
        end
        out=ismember_cell(val,obj.KEY);
    end
%% OLD
end
methods(Access=protected)
%% DISPLAY
    function out=getHeader(obj)
        dim = matlab.mixin.CustomDisplay.convertDimensionsToString(obj);
        name = matlab.mixin.CustomDisplay.getClassNameForHeader(obj);
        out=['  ' dim ' ' name newline];
    end
    function out=getFooter(obj)
        if isempty(obj.TABLE)
            out='';
            return
        end
        out=[];
        sz=size(obj);
        table=obj.TABLE;
        key=obj.KEY;

        NPRINT=40;

        if sz(1) > NPRINT
            table=cellfun(@(x) x(1:NPRINT,:),table,'UniformOutput',false);
            rEnd=[newline '    ...' newline];;
        else
            rEnd=newline;
        end

        if sz(2) > 20
            table=table(:,1:20);
            key=key(:,1:20);
            cEnd='...';
        else
            cEnd='';
        end

        kW=cellfun(@length,key);
        if all(cellfun(@isempty,obj.TABLE))
            tableStr='';
            keyStr=[strjoin(key,' ') ' '];
        else
            [tableStr,w,wo]=Table.cell2strTable(table,2,4,kW);
            keyStr=cell(length(key),1);
            wo(wo < 1)=0;
            minSzs=max([kW; wo],[],1);
            for i = 1:length(key)
                keyStr(i)=Table.space_fun(key(i),3,minSzs(i));
            end
            keyStr=join(keyStr,'');
            keyStr=[keyStr{1} cEnd];
        end

        %keyStr
        %w
        %kW
        %dk
        div=['    ' repmat('_',1,length(keyStr)-1) newline];
        out=['    ' keyStr newline  div tableStr rEnd];

        %display(obj.Table)
        %key=join(obj.KEY);
        %key=key{1};
        %c=numel(key);
        %sep='  ';
        %div=[ newline sep repmat('_',1,c) newline];
        %out=[div ...
        %     sep key newline ...
        %     div];
    end
    function out=displayEmptyObject(obj)
        display([obj.getHeader() newline obj.getFooter()]);
    end

end
methods(Hidden)
    function out=get_footer(obj)
        out=obj.getFooter();
    end
    function KEY=get_key(obj)
        KEY=obj.KEY;
    end
    function KEY=get_TABLE(obj)
        TABLE=obj.TABLE;
    end
end
methods(Static)
    function [txt,w,wo]=cell2strTable(C,nspace,indent,minSzs)
        %C={'bin','1','2'; 'val','1',''};
        %
        if ~exist('nspace','var') || isempty(nspace)
            nspace=2;
        end
        if ~exist('minSzs','var')
            minSzs=[];
        end

        txt=cell(size(C,1),1);
        col=zeros(1,size(C,1));
        r=zeros(1,size(C,1));
        cind=cellfun(@iscell,C);
        for i = 1:size(C,2)
            flds=C(:,i);
            %if cind(i)
            %    flds=C{:,i};
            %else
            %    flds=C(:,i);
            %end
            if iscell(flds) && numel(flds)==1 && iscell(flds{1})
                flds=flds{1};
            end

            ninds=cellfun(@isnumeric,flds);
            linds=cellfun(@islogical,flds);
            %cinds=cellfun(@Args.ischarcell,flds);
            oinds=cellfun(@isobject,flds) | cellfun(@iscell,flds);

            if all(ninds) && ~all(cellfun(@isempty,flds))
                flds=cellfun(@Num.toStr,flds,'UniformOutput',false);
                flds=split(flds,newline);
            else

                J=find(ninds);
                for jj = 1:length(J)
                    j=J(jj);
                    flds{j}=Num.toStr(flds{j});
                end

                J=find(oinds);
                for jj = 1:length(J)
                    j=J(jj);
                    sz=strrep(Num.toStr(size(flds{j})),',',char(215));
                    flds{j}=[sz ' ' class(flds{j})];
                end

                J=find(linds);
                for jj = 1:length(J)
                    j=J(jj);
                    if flds{j}
                        str='true';
                    else
                        str=false;
                    end
                    sz=strrep(Num.toStr(size(flds{j})),',',char(215));
                    flds{j}=strrep(Num.toStr(flds{j}),'1','true');
                    flds{j}=strrep(flds{j},'0','false');
                    flds{j}=strsplit(flds{j},newline)';
                    flds=flds{j};
                end

            end
            if i==size(C,2)
                n=0;
            else
                n=nspace;
            end

            [txt{i},col(i),wo(i)]=Table.space_fun(flds,n,minSzs(i));
        end

        w=col;

        txt=join(join([txt{:}],2),newline);
        txt=txt{1};
        if exist('indent','var') && ~isempty(indent) && indent~=0
            indnt=repmat(' ',1,indent);
            txt=strrep([indnt txt],newline,[newline indnt]);
        end

    end
    function [flds,col,colO]=space_fun(flds,n,minSz)
        col=max(cellfun(@(x) size(x,2), flds));
        colO=col;
        if ~exist('minSz','var') || isempty(minSz)
            minSz=0;
        end
        n=n-1;
        n(n<0)=0;
        for i = 1:length(flds)
            tmp=minSz-col;
            if tmp > 0
                col=tmp+col;
            end

            space=repmat(' ',1,n+col-size(flds{i},2));
            flds{i}=[flds{i} space];
        end
    end
end
methods(Static)
    function T=from_struct(S)
        keys=Struct.getFields(S);
        KEY=cellfun(@(x) strjoin(x,'.'),keys,'UniformOutput',false);

        TABLE=cellfun( @(x) getfield(S,x{:}),keys,'UniformOutput',false);

        SIZES=cellfun(@size,TABLE,'UniformOutput',false)';
        n=cellfun(@numel,SIZES);
        n=num2cell(max(n)-n);
        SIZES=cellfun(@(x,n) [x ones(1,n)],SIZES,n,'UniformOutput',false);
        SIZES=vertcat(SIZES{:});

        %uni=Set.isUniform(SIZES,1);
        %mx=max(SIZES,[],1);
        %mx=mx(uni);
        %if ~all(uni)
        %    error('Not all fields are of the same dimension')
        %end
        T=Table;
        TYPES=cellfun( @(x) class(getfield(S,x{:})),keys,'UniformOutput',false);

        T.KEY=KEY;
        T.TABLE=TABLE;
        T.types=TYPES;
        %[~,~,v]=unique(SIZES,'rows');
        %e=1:max(v);
        %counts=hist(v,e);

    end
    function obj=load(fname)
        S=load(fname);
        if ~isfield(S,'types')
            S.types=[];
        end
        obj=Table(S.table,S.key,S.types);
    end

end
end
