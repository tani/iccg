% 
% For GNU Prolog
%
:- if(current_prolog_flag(dialect, gprolog)).
write_length(X, N, _) :-
  write_to_atom(A, X),
  atom_length(A, N).
append([], []).
append([L|Ls], As) :-
  append(L, Ws, As),
  append(Ls, Ws).
plus(A, B, C) :- C is A + B.
:- endif.

% 
% For YAP
%
:- if(current_prolog_flag(dialect, yap)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(charsio)).
write_length(X, N, _) :-
  write_to_chars(X, C),
  atom_chars(A, C),
  atom_length(A, N).
:- endif.

