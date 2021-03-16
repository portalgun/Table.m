classdef Table < handle & matlab.mixin.CustomDisplay
properties
end
properties(Access=private)
    TABLE
    KEY
    types
end
methods
    function obj=Table(table,key);
        obj.KEY=key;

        M=size(table,2);
        obj.types=cell(1,M);
        obj.TABLE=cell(1,M);
        for m = 1:M
            obj.types(1,m)=unique(cellfun(@class, table(:,m),'UniformOutput',false));

            if numel(obj.types(1,m))
                obj.TABLE{m}=table(:,m);
                if ismember(obj.types(1,m),{'double','logical','single','uint8','int8','uint16','int16','uint32','int32','uint64','int64'});
                    obj.TABLE{m}=cell2mat(obj.TABLE{m});
                end
            end
        end
    end
    function sz=size(obj,dim)
        sz=[numel(obj.TABLE) numel(obj.TABLE{1})];
        if exist('dim','var') && ~isempty(dim)
            sz=sz(dim);
        end
    end
    function obj=subsasgn(obj,S,val)
        s=S.subs;
        flds=s(cellfun(@ischar,s));
        n=   cell2mat(s(cellfun(@isnumeric,s)));
        m=obj.get_cols(flds);

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
    function t=subsref(obj,S)
        n=[];
        S=S.subs;
        flds=S(cellfun(@ischar,S));
        n=   S(cellfun(@isnumeric,S));
        n=horzcat(n{:});

        m=obj.get_cols(flds);

        if isempty(n)
            n=1:size(obj,1);
        end
        col=obj.TABLE(:,m);
        for i = 1:length(col)
            table{i}=col{i}(n,:);
        end
        key=obj.KEY(m);
        t=Table(table,key);
    end
    function m=get_cols(obj,flds)
        if isempty(flds)
            m=1:size(obj,2);
            return
        end

        m=zeros(length(flds),1);
        for i = 1:length(flds)
            m(i)=find(cellfun(@(x) strcmp(x,flds{i}),obj.KEY));
        end
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

        if sz(1) > 100
            table=table(1:100,:);
            rEnd=['...' newline];;
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

        [tableStr,w]=Table.cell2strTable(table,2,4);
        keyStr=cell(length(key),1);
        for i = 1:length(key)
            keyStr(i)=Table.space_fun(key(i),w(i)-1);
        end
        keyStr=join(keyStr,'');
        keyStr=[keyStr{1} rEnd];
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
        out=[];
    end

end
methods(Static)
    function [txt,w]=cell2strTable(C,nspace,indent)
        %C={'bin','1','2'; 'val','1',''};
        %
        if ~exist('nspace','var') || isempty(nspace)
            nspace=2;
        end
        nspace=nspace-1;

        txt=cell(size(C,1),1);
        col=zeros(1,size(C,1));
        for i = 1:size(C,2)
            flds=C(:,i);
            ninds=cellfun(@isnumeric,flds);
            if all(ninds) && ~all(cellfun(@isempty,flds))

                flds=cellfun(@num2strSane,flds,'UniformOutput',false);
                flds=split(flds,newline);
            else
                J=find(ninds);
                for jj = 1:length(J)
                    j=J(jj);
                    flds{j}=num2strSane(flds{j});
                end

            end
            if i==size(C,2)
                n=0;
            else
                n=nspace;
            end
            [txt{i},col(i)]=Table.space_fun(flds,n);
        end
        w=col+nspace;
        txt=join(join([txt{:}],2),newline);
        txt=txt{1};
        if exist('indent','var') && ~isempty(indent) && indent~=0
            indnt=repmat(' ',1,indent);
            txt=strrep([indnt txt],newline,[newline indnt]);
        end

    end
    function [flds,col]=space_fun(flds,n)
        col=max(cellfun(@(x) size(x,2), flds))+n;
        for i = 1:length(flds)
            space=repmat(' ',1,col-size(flds{i},2));
            flds{i}=[flds{i} space];
        end
    end

end
end
