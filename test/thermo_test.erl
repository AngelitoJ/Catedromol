-module(thermo_test).
-compile(export_all).
-import(nosehoover_tools,[bath/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif. 

-include("constants.hrl").
-include("records.hrl").


% # The relaxation factor range between 0.5 and 2 Ps generally  #
% thermo = Thermostat()                                         #
% freq = 1./(2.2e1/my.au_time)                                                                 
% thermo.Q1 = 3*numat*Temper*my.kb/(freq**2)                    #  
% thermo.Q2 = Temper/my.kb/(freq**2)   

thermo_test() -> 
  Freq =  ?AU_TIME/2.2e1,
  Th1  = #thermo{q1 =3 * 4 * 100.0 * ?KB/(Freq*Freq), q2= 100*Freq*Freq/?KB },
  Th2  = nosehoover_tools:bath(Th1,0.1,100.0,1.0,4),
  A    = 1.0000007937707083,
  B    = Th2#thermo.scale,
  R    = math:sqrt(math:pow(B-A,2)),
  R < 1.0e-8. 

