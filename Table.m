classdef Table < handle & matlab.mixin.CustomDisplay
properties
end
properties(Access=private)
    TABLE
    KEY
    types
end
properties(Access=private,Constant)
    numTypes={'double','logical','single','uint8','int8','uint16','int16','uint32','int32','uint64','int64'};
end
methods
    function obj=Table(table,key);
        if numel(key)~=size(table,2);
            error('Key and table dimensions do not match');
        end
        obj.KEY=key;
        if ~iscell(table)
            table=num2cell(table);
        end

        M=size(table,2);
        obj.types=cell(1,M);
        obj.TABLE=cell(1,M);
        for m = 1:M
            obj.types(1,m)=unique(cellfun(@class, table(:,m),'UniformOutput',false));

            if numel(obj.types(1,m))
                obj.TABLE{m}=table(:,m);
                if ismember(obj.types(1,m), Table.numTypes);
                    obj.TABLE{m}=cell2mat(obj.TABLE{m});
                end
            end
        end
    end
    function out=length(obj)
        out=obj.size(1);
    end
    function out=width(obj)
        out=obj.size(2);
    end
    function sz=size(obj,dim)
        sz=[numel(obj.TABLE{1}) numel(obj.TABLE)];
        if exist('dim','var') && ~isempty(dim)
            sz=sz(dim);
        end
    end
    function obj=subsasgn(obj,S,val)
        s=S.subs;
        flds=s(cellfun(@ischar,s));
        n=   cell2mat(s(cellfun(@isnumeric,s)));
        m=obj.get_cols_ind(flds);

        if isempty(n)
            n=1:size(obj,1);
        end
        for ii = 1:length(m)
            i=m(ii);

            sz=size(obj.TABLE{i}(n,:));
            if numel(val)==1 || isequal(sz,size(val))
                obj.TABLE{i}(n,:)=val;
            elseif isequal(sz, size(val(:,ii)))
                obj.TABLE{i}(n,:)=val(:,ii);
            else
                error('Invalid assignment');
            end
        end

    end
    function S=struct(obj)
        [table,key]=obj.ret();
        S=struct();
        for i = 1:length(key)
            S.(key{i})=table(:,i);
        end
    end
    function [table,key]=ret(obj)
        table=obj.TABLE;
        key=obj.KEY;

        if Set.isUniform(obj.types)
            table=horzcat(table{:});
        end
    end
    function out=isnotkey(obj,val)
        %TODO expand this
        out=~obj.iskey(val) && ~ismember('char',obj.types);
    end
    function out=iskey(obj,val)
        if isnumeric(val)
            out=0;
            return
        end
        out=ismember(val,obj.KEY);
    end
    function varargout=subsref(obj,s)
        switch s(1).type
        case '.'
            if ismethod(obj,s(1).subs)
                str=['Table>Table.' s(1).subs];
                n=nargout(str);
            else
                n=nargout;
            end
            [varargout{1:n}] = builtin('subsref',obj,s);
        case '()'
            out=obj.subsref_ind([s(1).subs]);
            if length(s) > 1
                [varargout{1:nargout}]=subsref(out,s(2:end));
            else
                varargout{1}=out;
            end

        case '{}'
            error('Not a valid indexing expression');
        otherwise
            error('Not a valid indexing expression');
        end
    end
    function [C,ia,ic,counts]=unique(obj,varargin)
        if length(varargin) > 0
            o=obj.subsref_ind(varargin);
            t=o.TABLE;
        else
            t=obj.TABLE;
        end
        counts=[];
        [C,ia,ic]=cellfun(@unique,t,'UniformOutput',false);
        if numel(C)==1
            C=C{1};
            ia=ia{1};
            ic=ic{1};
            counts=transpose(hist(ic,unique(ic)));
        end

    end
    function ind=find(obj,varargin)
        subs=varargin;
        n=obj.get_n(subs);
        [keys,keyProps]=obj.get_key_and_props(varargin);
        m=obj.get_cols_ind(keys);
        [limit,ind,bAll]=obj.get_limits(n,keys,keyProps);
        ind=find(all([n ind],2)); % XXX

    end
    function n=get_n(obj,subs)
        indInds=logical(cumprod(cellfun(@(x) isnumeric(x), subs)));
        n=subs(indInds);
        n=horzcat(n{:});

        N=transpose(1:size(obj,1));
        bValid=ismember(n,N);
        if isempty(n)
            n=N;
        elseif all(bValid)
            n=ismember(N,n);
        else
            error(['Invalid index/indeces ' num2str(unique(n(~bValid)))]);
        end
    end
    function [keys,keyProps]=get_key_and_props(obj,subs)
        keyInds=cellfun(@(x) ischar(x) && iskey(obj,x), subs);
        notKey=subs(cellfun(@(x) ischar(x) && isnotkey(obj,x), subs));
        if ~isempty(notKey)
            error(['Invalid column names:' newline '        ' strjoin(notKey,', ') '.' ]);
        end
        keys=subs(keyInds);
        keyI=find(keyInds);
        keyProps=cell(1,length(keyI));
        for i = 1:length(keyI)-1
            keyProps{i}=subs(keyI(i)+1:keyI(i+1)-1);
        end
        if ~isempty(keyI) && keyI(end) ~= length(subs)
            keyProps{end}=subs(keyI(end)+1:end);
        end
    end
    function t=subsref_ind(obj,subs)
        n=obj.get_n(subs);
        [keys,keyProps]=obj.get_key_and_props(subs);
        [limit,ind,bAll]=obj.get_limits(n,keys,keyProps);

        ind=all([n ind],2); % XXX

        if bAll==0
            table=cellfun(@(x) x(ind) ,obj.TABLE(limit),'UniformOutput',false);
            key=obj.KEY(limit);
        else
            table=cellfun(@(x) x(ind) ,obj.TABLE,'UniformOutput',false);
            key=obj.KEY;
        end
        t=Table(table,key);

    end

    function [limit,ind,bAll]=get_limits(obj,n,keys,keyProps)
        m=obj.get_cols_ind(keys);
        col=obj.TABLE(:,m);

        bAll=1;
        limit=[];
        ind=true(size(obj));
        for i = 1:length(keys)
            kp=keyProps{i};
            if isempty(kp)
                bAll=0;
                limit=[limit m(i)];
                continue
            elseif isnumeric(kp{1}) || ~ismember(kp{1},{'==','>','<','>=','<=','~='})
                kp(2:end+1)=kp;
                kp{1}='==';
            end
            if length(kp) < 2
                kp(end+1)='|';
            elseif isnumeric(kp{2})  || ismember(kp{2},{'|','&'})
                kp(3:end+1)=kp(2:end);
                kp{2}='|';
            end
            mod=kp{1};
            bin=kp{2};
            str=['col{'  num2str(i) '} ' mod ' X ' bin ' '];

            % funcitonality for array indexing
            sz=obj.get_col_size(keys{i});
            I=find(cellfun(@(x) isnumeric(x) && numel(x) > 1,kp));
            if ~isempty(I)
            for j=transpose(fliplr(I));
                if ~isequal(size(kp{j}),sz) && j>1 && j < numel(kp);
                    kp=[kp(1:j-1) num2cell(kp{j}) kp(j+1:end)];
                elseif ~isequal(size(kp{j}),sz) && j>1
                    kp=[kp(1:j-1) num2cell(kp{j})];
                elseif ~isequal(size(kp{j}),sz) j < numel(kp);
                    kp=[num2cell(kp{j}) kp(j+1:end)];
                end
            end
            end

            STR='';
            for j = 3:length(kp)
                val=kp{j};
                if isnumeric(val)
                    val=num2str(val);
                end
                STR=[STR strrep(str,'X',val)];
            end
            STR=[STR(1:end-3) ';'];
            ind(:,i)=eval(STR); % XXX
        end
    end
    function sz=get_col_size(obj,key)
        KI=ismember(obj.KEY,key);
        sz=size(obj.TABLE{KI});
        sz=sz(2:end);
    end

    function m=get_cols_ind(obj,flds)
        if isempty(flds)
            m=1:size(obj,2);
            return
        end

        m=zeros(length(flds),1);
        for i = 1:length(flds)
            m(i)=find(cellfun(@(x) strcmp(x,flds{i}),obj.KEY));
        end
    end
    function add_row(obj,key,data)
        % XXX
    end
    function obj=sort_rows(obj,fld,bReverse)
        % XXX
    end
    function obj=sort_columns(obj,type,bReverse)
        % XXX
    end
end
methods(Access=protected)
    %function getHeader(obj)
    %end
    %function getFooter(obj)
    %end
    function out=getHeader(obj)
        dim = matlab.mixin.CustomDisplay.convertDimensionsToString(obj);
        name = matlab.mixin.CustomDisplay.getClassNameForHeader(obj);
        out=['  ' dim ' ' name newline];
    end
    function out=getFooter(obj)
        out=[];
        sz=size(obj);
        table=obj.TABLE;
        key=obj.KEY;

        NPRINT=40;

        if sz(1) > NPRINT
            table=cellfun(@(x) x(1:NPRINT,:),table,'UniformOutput',false);
            rEnd=[newline '    ...' newline];;
        else
            rEnd='';
        end

        if sz(2) > 20
            table=table(:,1:20);
            key=key(:,1:20);
            cEnd='...';
        else
            cEnd='';
        end

        kW=cellfun(@length,key);
        [tableStr,w,wo]=Table.cell2strTable(table,2,4,kW);
        keyStr=cell(length(key),1);
        wo(wo < 1)=0;
        minSzs=max([kW; wo],[],1);
        for i = 1:length(key)
            keyStr(i)=Table.space_fun(key(i),3,minSzs(i));
        end
        %keyStr
        %w
        %kW
        %dk
        keyStr=join(keyStr,'');
        keyStr=[keyStr{1} cEnd];
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
        for i = 1:size(C,2)
            flds=C(:,i);
            ninds=cellfun(@isnumeric,flds);
            if all(ninds) && ~all(cellfun(@isempty,flds))

                flds=cellfun(@Num.toStr,flds,'UniformOutput',false);
                flds=split(flds,newline);
            else
                J=find(ninds);
                for jj = 1:length(J)
                    j=J(jj);
                    flds{j}=Num.toStr(flds{j});
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
end
