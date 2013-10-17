Catedromol

A Erlang distributed molecular dynamics tool


This tool features Nosé-Hoover constant temperature molecular dynamics in a distributed fashion
as an concurrent actors based simulation. The whole system is partitioned among participants atoms
forming 4-atoms unit following designatedbonds and internal coordinates, of chemical interes and those
units comunicate by message passing along the simulation. 

We are using rebar packaging and a concise use of geopt like facilities to build a selfconatined standalone
executable that can be used as a normal Unix application (provided you have the erlang runtime somewhere).

See the built-in instructions to the available modes of execution...


1- Install erlang

2- Try catedromol

  Usage: catedromol -d 2 -D ./data/metano metano -I 1000 --Relaxation 100 2> metano.log 

produces a nose-hoover dynamics and ouputs a molden XYZ format at metano.log


