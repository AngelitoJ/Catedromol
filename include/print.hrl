
-ifdef(TEST).
-define(Console(Format,Args), io:format(Format,Args)).
-define(Console(Args),        io:format(Args)).
-define(GroupInfo(Args),      io:format("~p",[Args])).
-define(MuCInfo(Args),      io:format("~p",[Args])).
-define(WilsonInfo(Args),      io:format("~p",[Args])).
-define(POUTPUT(Format,Args),     io:format(Format,Args)).
-define(PDEBUG(Format,Args),     io:format(Format,Args)).
-define(PINFO(Format,Args),      io:format(Format,Args)).
-define(PWARNING(Format,Args),   io:format(Format,Args)).
-else.
-define(Console(Format,Args), gen_server:call(sim_console, {print, io_lib:format(Format,Args) },infinity)).
-define(Console(Args),        gen_server:call(sim_console, {print, Args },infinity)).
-define(GroupInfo(Args),      case allowed_to_print(info) of true -> gen_server:call(sim_console, {ginfo, Args },infinity); false -> none end ).
-define(MuCInfo(Args),        case allowed_to_print(info) of true -> gen_server:call(sim_console, {mucinfo, Args },infinity); false -> none end ).
-define(WilsonInfo(Args),     gen_server:call(sim_console, {winfo, Args },infinity)).
-define(POUTPUT(Format,Args),     print(output,Format,Args)).
-define(PDEBUG(Format,Args),     print(debug,Format,Args)).
-define(PINFO(Format,Args),      print(info,Format,Args)).
-define(PWARNING(Format,Args),   print(warning,Format,Args)).

-endif.

% print(Nivel,Formato,Argumentos) ->
%     print(Nivel,Formato,Argumentos).

allowed_to_print(Nivel) ->
   case Nivel of
      output -> true;
      Other -> lists:member(Other,get(reporting_level))
   end.


print(Nivel,Formato,Argumentos) ->
   case allowed_to_print(Nivel) of
   true ->
      Msg = io_lib:format(Formato,Argumentos),
      ?Console(Msg);
   false ->
      none
   end.

%     case Nivel of
%     output ->
%         Msg = io_lib:format(Formato,Argumentos),
%          ?Console(Msg);
%     Otro ->
%         case lists:member(Otro,Niveles) of
%         true ->
% %             Etiqueta = atom_to_list(Otro),
% %             Msg = io_lib:format("[~s] "++Formato,[Etiqueta|Argumentos]),
%             Msg = io_lib:format(Formato,Argumentos),
%             ?Console(Msg);
%         false -> 
%             ok
%         end
%     end.
