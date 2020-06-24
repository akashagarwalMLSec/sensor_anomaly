function [F1, F2, F3, F4,F5] = MANE_modifiedfor5layers(A1,A2,A3,A4,A5,D12,D13,D23,D14,D15,D24,D25,D34,D35,D45,alpha,d1,d2,d3,d4,d5,maxIter)
    % A1: adjacency matrix of the 1st layer (size: n1-by-n1)
    % A2: adjacency matrix of the 2nd layer (size: n2-by-n2)
    % A3: adjacency matrix of the 3rd layer (size: n3-by-n3)
    % A4: adjacency matrix of the 4th layer (size: n4-by-n4)
    % A5: adjacency matrix of the 5th layer (size: n5-by-n5)
    % D12: cross-layer matrix between the 1st and 2nd layers (size: n1-by-n2)
    % D13: cross-layer matrix between the 1st and 3rd layers (size: n1-by-n3)
    % D14: cross-layer matrix between the 1st and 4th layers (size: n1-by-n2)
    % D15: cross-layer matrix between the 1st and 5th layers (size: n1-by-n3)
    % D23: cross-layer matrix between the 2nd and 3rd layers (size: n2-by-n3)
    % D24: cross-layer matrix between the 2nd and 4th layers (size: n1-by-n2)
    % D25: cross-layer matrix between the 2nd and 5th layers (size: n1-by-n3)
    % D34: cross-layer matrix between the 3rd and 4th layers (size: n1-by-n2)
    % D35: cross-layer matrix between the 3rd and 5th layers (size: n1-by-n3)
    % D45: cross-layer matrix between the 4th and 5th layers (size: n1-by-n2)
    
    % alpha: balancing parameter 
    % d1: embedding dimension of the nodes in the 1st layer 
    % d2: embedding dimension of the nodes in the 2nd layer 
    % d3: embedding dimension of the nodes in the 3rd layer 
    % d4: embedding dimension of the nodes in the 4th layer 
    % d5: embedding dimension of the nodes in the 5th layer 
    
    % maxIter: maxium number of iterations
    
    opts.disp = 0;
    n1 = size(A1,1);
    n2 = size(A2,1);
    n3 = size(A3,1);
    n4 = size(A4,1);
    n5 = size(A5,1);
    F1 = orth(rand(n1,d1));
    F2 = orth(rand(n2,d2));
    F3 = orth(rand(n3,d3));
    F4 = orth(rand(n4,d4));
    F5 = orth(rand(n5,d5));


    diag1 = sum(A1,2);
    diag2 = sum(A2,2);
    diag3 = sum(A3,2);
    diag4 = sum(A4,2);
    diag5 = sum(A5,2);

    M1 = full(diag(diag1.^-0.5)); M1(isinf(M1))=0;
    M2 = full(diag(diag2.^-0.5)); M2(isinf(M2))=0;
    M3 = full(diag(diag3.^-0.5)); M3(isinf(M3))=0;
    M4 = full(diag(diag4.^-0.5)); M4(isinf(M4))=0;
    M5 = full(diag(diag5.^-0.5)); M5(isinf(M5))=0;

    L1 = M1*A1*M1; 
    L2 = M2*A2*M2;
    L3 = M3*A3*M3;
    L4 = M4*A4*M4;
    L5 = M5*A5*M5;

    iter = 1;
    while iter <= maxIter
        % calculate M
        M1 = L1+alpha*(D12'*F2*F2'*D12 + D13*F3*F3'*D13' + D14*F4*F4'*D14' + D15*F5*F5'*D15'); 
        M2 = L2+alpha*(D12'*F1*F1'*D12 + D23*F3*F3'*D23' + D24'*F4*F4'*D24 + D25*F5*F5'*D25'); 
        M3 = L3+alpha*(D13'*F1*F1'*D13 + D23'*F2*F2'*D23 + D34'*F4*F4'*D34 + D35'*F5*F5'*D35);
        M4 = L4+alpha*(D14'*F1*F1'*D14 + D24*F2*F2'*D24' + D34'*F3*F3'*D34 + D45*F5*F5'*D45'); 
        M5 = L5+alpha*(D15'*F1*F1'*D15 + D25'*F2*F2'*D25 + D35'*F3*F3'*D35 + D45'*F4*F4'*D45); 


        M1 = (M1+M1')/2;
        M2 = (M2+M2')/2;
        M3 = (M3+M3')/2;
        M4 = (M4+M4')/2;
        M5 = (M5+M5')/2;

        % update F
        [F1,Daeig1] = eigs(M1,d1,'LA',opts);
        [F2,Daeig2] = eigs(M2,d2,'LA',opts);
        [F3,Daeig3] = eigs(M3,d3,'LA',opts);
        [F4,Daeig4] = eigs(M4,d4,'LA',opts);
        [F5,Daeig5] = eigs(M5,d5,'LA',opts);

        iter = iter + 1;
    end
end

