%% Erlang Nose Hoover dynamics simulator 
%% General records defintions
%% @ 2012 Felipe Z. Initial setup from python version
%% @ 2012 Angel Alvarez Include records info

-type(atomconex()      :: bondconex|angleconex|dihedralconex ).
-type(bondconex()         :: {bond, integer(), integer()}).
-type(angleconex()        :: {angle, integer(), integer(), integer()}).
-type(dihedralconex()     :: {dihedral, integer(), integer(), integer(), integer()}).
-type(force3d()         :: [float()] ).

%%% RECORDS
-record(molecule,
   {
      index     = 0  :: pos_integer(),
      globals   = [] :: [pos_integer()],
      labels    = [],
      position_cart      = [],      % Current position in cartesian coordinates
      position_int       = [],      % 
      q0                 = [],      % Internal coordinates at point Q0 (minimun of energy)
      grad_in_Q0         = [],      % Initial gradient at point Q0 in internal coordinates,only proper elements
      grad_in_Qi         = [],      % Current gradient at point Qi in internal coordinates only proper elements  
      hess               = [],      % Full Hessian rows (one for every conex elemnent this group has, after reducing to proper components )
      vel       = [],
      mass      = [],
      forces    = [[0.0,0.0,0.0],[0.0,0.0,0.0],[0.0,0.0,0.0],[0.0,0.0,0.0]]  :: [force3d()],
      conex              = []          :: [atomconex()],  % conectivity for proper atoms (private and managed)
      fullconex          = []          :: [atomconex()],  % full conectivity (all atoms )
      globalconex        = []          :: [atomconex()],  % conectivity (proper atoms in global)
      members            = {[],[],[]}  :: {[pos_integer()],[pos_integer()],[pos_integer()]},
      wilson    = [],
      eK        = 0.0,                 % Kinectic Energy for this group
      eP        = 0.0,                 % Potential Energy for this group ( only Hessian part)
      qt_1      = []
    }).



-record(system,
	{
        atoms,
        masses,
        cartesian,
        velocities,
        gradient,
        hessian
    }).

-record(atominfo,{ 
                  localidx   = 0                               % Local index [1..4]
                  ,globalidx = 0                               % Global index [1..n]
                  ,status    = undefined                       % private|managed|slaved
                  ,label     = undefined                       % "C1", "H2", "U96" :-)
                  ,position  = [0.0,0.0,0.0]                   % list of Cartesian Coordinates
                  ,velocity  = [0.0,0.0,0.0]                   % Velocitiy in Au/s
                  ,force     = [0.0,0.0,0.0]                   % force exerted over this atom
                  ,mass      = 1                               % Atom Mass relative to electron
                  }).

% MuC: Minimal Unit of Computation
% This is the molecule record equivalence for autonomous process holding atom cuartets data
-record(muc,{
            index       = 0      :: integer()      % index of this MuC
            ,atomdata   = []     :: [#atominfo{}]  % Atom data for atoms on this MuC
            ,q0         = [0.0]  :: [float()]      % Initial position (internal coordinates)
            ,qi         = [0.0]  :: [float()]      % Current position (internal coordinates)
            ,grad_q0    = [0.0]  :: [float()]      % gradient in q0 
            ,grad_qi    = [0.0]  :: [float()]      % gradient in qi
            ,hess       = [0.0]  :: [float()]      % Hessian Data
            ,gconex     = []     :: [atomconex()]  % Global proper conex elements for this MuC
            ,lconex     = []     :: [atomconex()]  % Local proper conex elements for this MuC
            }).

% MuC process state
% MuC entities have memory so if you try hurting them, they will recall you upon future encounters, taking revenge...
-record(mucstate,{
            mucdata     = undefined :: #muc{}   % MuC contained in this process
            }).