
-record(thermo,
        {
            q1 = 0.0        ::float() , 
            q2 = 0.0        ::float(),
            x1 = 0.0        ::float(), 
            vx1 = 0.0       ::float(),
            x2 = 0.0        ::float(), 
            vx2 = 0.0       ::float(),
            scale = 1.0     ::float()
        }).


% Setup thermostate
-spec thermo_init(float(),integer()) -> #thermo{}.
thermo_init(T,Numat) when is_float(T), is_integer(Numat) ->
  Freq =  ?AU_TIME/2.2e1,
  #thermo{q1 =3 * Numat * T * ?KB/(Freq*Freq), q2= T*Freq*Freq/?KB }.

%%% Thermostat and Integrators
% The relaxation factor range between 0.5 and 2 Ps generally  
% -spec bath(#thermo{},float(),float(),float()) -> #thermo{}.
bath(#thermo{q1=Q1,q2=Q2,vx1=Vx1,vx2=Vx2,x1=X1,x2=X2} = Thermo,DT,T,Ek,Numat) ->

    DT2 = DT*0.5, DT4 = DT*0.25, DT8 = DT*0.125,
  
    G2 = (Q1*math:pow(Vx1,2) -T*?KB)/Q2,                %Thermo#thermo{vx2 += G2*DT4 = R1}
    R1 = Vx2 + G2 *DT4, 
    R2 =Vx1*math:exp(-R1*DT8),                          %Thermo#thermo{vx1 *= expvx2*DT8= R2}  
    G1 = (2*Ek - 3* Numat *T *?KB )/Q1,
    R3 = R2+G1*DT4,                               %Thermo#thermo{vx1 += G1*DT4 = R3},         
    R4 = R3*math:exp(-R1*DT8),                         %Thermo#thermo{vx1 *= exp(vx2*DT8)= R4} 
    S  = math:exp(-R4*DT2),                             %Thermo#thermo{scale = exp(R4*DT2)= Scale} 
      
    Ek_new = Ek*S*S,
    
    Rx1 = X1 + R4* DT2,                          %Thermo#thermo{x1  = X1 + vx1*DT2 = Rx1}                            
    Rx2 = X2 + R1* DT2,                          %Thermo#thermo{x2  = x2 + vx2*DT2 = Rx2} 
    R5 = R4*math:exp(-R1*DT8),                        %Thermo#thermo{vx1 *= exp(vx2*DT8)= R5}
    G1a = (2*Ek_new -3* Numat * T * ?KB)/Q1,
    R6 = R5 + G1a*DT4,                           %Thermo#thermo{vx1 += G1*DT4 = R5}
    R7 = R6*math:exp(-R1*DT8),                        %Thermo#thermo{vx1 *= exp(vx2*DT8)=R7}
    G2a = (Q1*R7*R7 - T*?KB)/Q2,                
    R8 = R1+ G2a*DT4,                            %Thermo#thermo{vx2 += G2*DT4 = R8}


    Thermo#thermo{vx1 = R7, vx2 = R8, x1 = Rx1, x2 = Rx2, scale = S }.
