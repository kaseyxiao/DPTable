function Z=RandomInit(n,m)
    Z=rand(n,m);
    Z=Z./repmat(sum(Z,2),1,m);
%     p=randperm(n);
%     quotient=floor(n/m);
%     for i=1:m
%         Z(p(quotient*(i-1)+1:quotient*i),i)=1;
%     end
%     if mod(n,m)~=0
%         for i=quotient*m+1:n
%             Z(p(i),i-quotient*m)=1;
%         end
%     end