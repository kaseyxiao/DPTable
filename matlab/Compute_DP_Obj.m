function result=Compute_DP_Obj(log_p,log_card,Z,O)
    assign=GetAssignment(Z);
    tmp=sum(assign,1);
    index=find(tmp>0);
    result=log_p*assign(:,index)-log_card*O*assign(:,index)+log(sum(assign(:,index),1))+sum(log_card);
    result=lnsumexp(result)+2*log(length(index));
    clear assign;