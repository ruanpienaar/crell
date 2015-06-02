-module(mod_a).
-export([
    func_b/0,
    func_c/0,
    func_d/0
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