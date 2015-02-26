function FindOptimalMerging8(input_file,m,lambda,output_file)
%input_file: the file name for the input
%m: the number of clusters
%lambda: the positive regularization parameter to trade-off between the DP cost and the maximum dissimilarity among cluster assignments.
%output_file: the file name for the output
    [node_card,clique]=GetFileContent(input_file);
    d=length(node_card);
    n=length(clique);
    if n<=m
        error('The number of cliques should be larger than the number of clusters!!!');
    end
    O=zeros(d,n);
    log_p=zeros(1,n);
    for i=1:n
        index=clique{i};
        O(index,i)=1;
        log_p(i)=sum(log(node_card(index)));
        clear index;
    end
    log_node_card=log(node_card);
    sum_log_node_card=sum(log_node_card);
    M=ConstructDifferenceOperator(m);
    max_iter=20;
%     Z=ones(n,m)/m;
    Z=RandomInit(n,m);
%     W=Compute_Weights(Z);
%     best_assignment=Z;
%     best_obj=Compute_DP_Obj(log_p,log_node_card,Z,O);
    cvx_setup;
    cvx_quiet(true);
    cvx_expert true;
    cvx_solver sedumi;
%     display('Initial value for Z:');
%     display(Z);
    for iter=1:max_iter
        display(['Iteration',blanks(1),num2str(iter),blanks(1),'is running...']);
        old_Z=Z;
        tmp1=sum(old_Z,1);
        tmp2=log(tmp1)-1+sum_log_node_card;
        tmp3=old_Z*M;
        cvx_begin
            variables Z(n,m) t(1,m) r;
            minimize log_sum_exp(t)-lambda*r;%+trace(W'*Z);
            subject to
                Z>=0;
                Z*ones(m,1)==ones(n,1);
                r*ones(1,m*(m-1)/2)-2*ones(1,n)*((Z*M).*tmp3)+sum(tmp3.^2,1)<=0;
                ones(1,n)*Z>=1;
                log_p*Z-t-log_node_card*O*Z+tmp2+ones(1,n)*Z./tmp1==0;
        cvx_end
        clear tmp1 tmp2 tmp3;
%         current_obj=Compute_DP_Obj(log_p,log_node_card,Z,O);
%         if current_obj<best_obj
%             best_obj=current_obj;
%             best_assignment=Z;
%         end
        clear old_Z;
    end
%     Z=best_assignment;
%     disp(['Best DP objective value:',blanks(1),num2str(best_obj)]);
    result_cell=GetFinalResult(Z,O);
    if nargin==4&&~isempty(output_file)
        fid=fopen(output_file,'w');
        for i=1:length(result_cell)
            string=GenerateString(result_cell{i});
            fprintf(fid,'%s\n',string);
        end
        fclose(fid);
    end
    clear M node_card clique O log_p log_node_card;
    
function result=GenerateString(array)
    result='';
    for i=1:length(array)
        result=[result,blanks(1),num2str(array(i))];
    end
    
function W=Compute_Weights(Z)
    [n,m]=size(Z);
    [~,index]=max(Z,[],2);
    index = sub2ind([n m], 1:n, index');
    W=zeros(n,m);
    W(index)=exp(Z(index));