A1 = csvread('ClaimsNumbertransformed.csv',1,1);
A2 = csvread('ClaimsAmounttransformed.csv',1,1);
A3 = csvread('Precipitationtransformed.csv',1,1);
A4 = csvread('HouseAgetransformed.csv',1,1);
A5 = csvread('CreditScoretransformed.csv',1,1);

D12 =csvread('AmtandNumbertransformed.csv',1,1);
D23 = csvread('PreciLpandAmttransformed.csv',1,1);
D13 = csvread('PrecipandNumbertransformed.csv',1,1);
D14 = csvread('NumberandHousetransformed.csv',1,1);
D15 = csvread('NumberandCredittransformed.csv',1,1);
D24 = csvread('AmtandHousetransformed.csv',1,1);
D25 = csvread('AmtandCredittransformed.csv',1,1);
%D34 = csvread('PrecipandHousetransformed.csv',1,1);
%D35 = csvread('PrecipandCredittransformed.csv',1,1);
%D45 = csvread('HouseandCredittransformed.csv',1,1);

alpha = 1
d1 = 30
d2 = 30
d3 = 30
d4 = 30
d5 = 30
maxIter = 10000

[F1,F2,F3,F4,F5]= MANE_modifiedfor5layersandremovedsomedependencies(A1,A2,A3,A4,A5,D12,D13,D23,D14,D15,D24,D25,alpha,d1,d2,d3,d4,d5,maxIter)

csvwrite('Claims_MANE30_Sep2.csv',F1)
csvwrite('ClaimAmount_MANE30_Sep2.csv',F2)
csvwrite('Precip_MANE30_Sep2.csv',F3)

csvwrite('HouseAge_MANE30_Sep2.csv',F4)
csvwrite('CreditScore_MANE30_Sep2.csv',F5)

