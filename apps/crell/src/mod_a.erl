-module(mod_a).
-export([
    func_b/0, % Single external call
    func_c/0, % Multiple external call's
    func_d/0, % Multiple external call's
    func_e/0, % builtin and Single external call
    func_f/0 % calling local function, that calls external function.
]).
func_b() ->
    mod_b:func_b().
func_c() ->
    mod_b:func_b(),
    mod_c:func_c().
func_d() ->
    mod_b:func_b(),
    mod_c:func_c(),
    mod_d:func_d().
func_e() ->
	now(),
	mod_b:func_b().
func_f() ->
	func_b().

% GET IT RUNNING:

% xref:start(x).
% xref:add_directory(x, "/Users/ruanpienaar/code/crell/apps/crell/ebin", [{builtins, true}]).
% xref:q(x, "E | ['mod_a']").

%% High Level
% mod_a 
%     - mod_b:func_b
%     - mod_c:func_c
%     - mod_d:func_d

%% expected response: 

% when asking for the possible execution branches:

% [{mod_a, [ {func_b, 0, 
%             [
%                 {mod_b, func_b, 0}
%             ]
%            },
%            {func_c , 0, 
%             [
%                 {mod_b, func_b, 0},
%                 {mod_c, func_c, 0}
%             ]
%            },
%            {func_c , 0, 
%             [
%                 {mod_b, func_b, 0},
%                 {mod_c, func_c, 0},
%                 {mod_d, func_d, 0}
%             ]
%            }
%          ]
% ]


% This seemed to work:

% 17> xref:q(x, "E | ['mod_a']").  
% {ok,[{{mod_a,func_b,0},{mod_b,func_b,0}},
%      {{mod_a,func_c,0},{mod_b,func_b,0}},
%      {{mod_a,func_c,0},{mod_c,func_c,0}},
%      {{mod_a,func_d,0},{mod_b,func_b,0}},
%      {{mod_a,func_d,0},{mod_c,func_c,0}},
%      {{mod_a,func_d,0},{mod_d,func_d,0}}]}
% 18> 