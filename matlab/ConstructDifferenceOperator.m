function M=ConstructDifferenceOperator(m)
    M=zeros(m,m*(m-1)/2);
    count=1;
    for i=1:m-1
        index=count:(count+m-i-1);
        M(i,index)=1;
        M(i+1:m,index)=-eye(m-i);
        count=count+m-i;
    end