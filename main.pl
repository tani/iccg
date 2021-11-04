:- include('ccg.pl').
:- include('compat.pl').
:- include('printer.pl').
:- initialization(main).

:- if(current_prolog_flag(dialect, swi)).

main :-
  findall(
    N,
    (between(3, 4, I), length(G, I), maplist(axiom, G, N), parse(cg(_), N, T), T=node('S' by _, _), freeze(G)),
    NS
  ),
  concurrent_maplist(
    parse(cg(3)),
    NS,
    TS
  ),
  concurrent_maplist(
    iparse(iqccg(3)),
    NS,
    US
  ),
  forall(
    between(1, N, I),
    (
      nth1(I, TS, T),
      nth1(I, US, U),
      write_chars(80, '='),
      draw_tree(T),
      draw_tree(U)
    )
  ),
  halt.

:- else.

main :-
  forall(
    (between(3, 4, I), length(G, I), maplist(axiom, G, N), parse(cg(_), N, T), T=node('S' by _, _), freeze(G)),
    (write_chars(80, '='), nl, draw_tree(T), iparse(qccg(3), N, U), U=node('S' by _, _), draw_tree(U))
  ),
  halt.

:- endif.
