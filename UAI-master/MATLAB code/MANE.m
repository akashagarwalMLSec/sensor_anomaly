function [F1, F2, F3] = MANE(A1, A2, A3, D12, D13, D23, alpha, d1, d2, d3, maxIter)
    % A1: adjacency matrix of the 1st layer (size: n1-by-n1)
    % A2: adjacency matrix of the 2nd layer (size: n2-by-n2)
    % A3: adjacency matrix of the 3rd layer (size: n3-by-n3)
    % D12: cross-layer matrix between the 1st and 2nd layers (size: n1-by-n2)
    % D13: cross-layer matrix between the 1st and 3rd layers (size: n1-by-n3)
    % D23: cross-layer matrix between the 2nd and 3rd layers (size: n2-by-n3)
    % alpha: balancing parameter 
    % d1: embedding dimension of the nodes in the 1st layer 
    % d2: embedding dimension of the nodes in the 2nd layer 
    % d3: embedding dimension of the nodes in the 3rd layer 
    % maxIter: maxium number of iterations
    
    opts.disp = 0;
    n1 = size(A1,1);
    n2 = size(A2,1);
    n3 = size(A3,1);
    F1 = orth(rand(n1,d1));
    F2 = orth(rand(n2,d2));
    F3 = orth(rand(n3,d3));

    diag1 = sum(A1,2);
    diag2 = sum(A2,2);
    diag3 = sum(A3,2);
    M1 = full(diag(diag1.^-0.5)); M1(isinf(M1))=0;
    M2 = full(diag(diag2.^-0.5)); M2(isinf(M2))=0;
    M3 = full(diag(diag3.^-0.5)); M3(isinf(M3))=0;

    L1 = M1*A1*M1; 
    L2 = M2*A2*M2;
    L3 = M3*A3*M3;

    iter = 1;
    while iter <= maxIter
        % calculate M
        M1 = L1+alpha*(D12*F2*F2'*D12'+D13*F3*F3'*D13'); 
        M2 = L2+alpha*(D12'*F1*F1'*D12+D23*F3*F3'*D23'); 
        M3 = L3+alpha*(D13'*F1*F1'*D13+D23'*F2*F2'*D23); 
        M1 = (M1+M1')/2;
        M2 = (M2+M2')/2;
        M3 = (M3+M3')/2;
        
        % update F
        [F1,Daeig1] = eigs(M1,d1,'LA',opts);
        [F2,Daeig2] = eigs(M2,d2,'LA',opts);
        [F3,Daeig3] = eigs(M3,d3,'LA',opts);

        iter = iter + 1;
    end
end

