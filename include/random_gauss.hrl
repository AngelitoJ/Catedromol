%  Box-Muller and Marsaglia gaussian number generators
%  2012 Angel Alvarez

%  These numbers obey a normal distribution with mean of 1.0 and a standard deviation of 0.5
-spec bm_gauss() -> float().
bm_gauss() ->
    bm_gauss(1.0,0.5).

%  These numbers obey a normal distribution with desired mean of DM and a desired standard deviation of DS
%  Box-Muller transform 
-spec bm_gauss(float(),float()) -> float().
bm_gauss(DM,DS) when is_float(DM), is_float(DS) ->
    Pi = math:pi(),   % Set PI 
    DM + ((math:sqrt(-2 * math:log(random:uniform())) * math:cos(2 * Pi * random:uniform())) * DS).

% Polar method
-spec polar_gauss() -> float().
polar_gauss() ->
    polar_gauss(1.0,0.5).

%  These numbers obey a normal distribution with desired mean of DM and a desired standard deviation of DS
%  Polar Method 
-spec polar_gauss(float(),float()) -> float().
polar_gauss(DM,DS) when is_float(DM), is_float(DS) ->
    U = (2*random:uniform()) - 1, 
    V = (2*random:uniform()) - 1,
    S = U*U + V*V,
    case (S > 1) or (S == 0.0) of
    true ->
        polar_gauss(DM,DS);
    false ->
        F = math:sqrt(-2*(math:log(S)/S)),
        DM + DS* F * U
    end.