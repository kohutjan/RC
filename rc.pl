:- use_module(library(lists)).

:- initialization main.

main :-
  read_lines(L),
  parseRubiksCube(L, C),
  rotateWhite(C, C1),
  rotateWhite(C1, C2),
  rotateWhite(C2, C3),
  rotateWhite(C3, C4),
  printRubiksCube(C4).

rotateWhite(cube([WTL, WTM, WTR, WML, WMM, WMR, WDL, WDM, WDR],
                 [RTL, RTM, RTR|RT],
                 [BTL, BTM, BTB|BT],
                 [OTL, OTM, OTO|OT],
                 [GTL, GTM, GTG|GT],
                  Y), RC) :-
  W = [WDL, WML, WTL, WDM, WMM, WTM, WDR, WMR, WTR],
  R = [BTL, BTM, BTB|RT],
  B = [OTL, OTM, OTO|BT],
  O = [GTL, GTM, GTG|OT],
  G = [RTL, RTM, RTR|GT],
  RC =.. [cube, W, R, B, O, G, Y].

%rotateRed :-

%rotateBlue :-

%rotateOrange :-

%rostateGreen :-

%rostateYellow :-

parseRubiksCube(L, C) :- parseWhiteSide(L, W),
                         parseMiddleSide(L, 0, R),
                         parseMiddleSide(L, 4, B),
                         parseMiddleSide(L, 8, O),
                         parseMiddleSide(L, 12, G),
                         parseYellowSide(L, Y),
                         C =.. [cube, W, R, B, O, G, Y].

printRubiksCube(cube(W, R, B, O, G, Y)) :-
  printSide(W),
  printMiddelSidesLine(R, B, O, G, 0),
  printMiddelSidesLine(R, B, O, G, 3),
  printMiddelSidesLine(R, B, O, G, 6),
  printSide(Y).


parseWhiteSide([T, M, D|_], W) :-
  append([T, M, D], W).

parseMiddleSide([_,_,_, TT, MM, DD|_], Drop, Side) :-
  drop(Drop, TT, TS),
  drop(Drop, MM, MS),
  drop(Drop, DD, DS),
  take(3, TS, T),
  take(3, MS, M),
  take(3, DS, D),
  append([T, M, D], Side).

parseYellowSide([_,_,_,_,_,_, T, M, D|_], Y) :-
  append([T, M, D], Y).

printSide([TL, TM, TR, ML, MM, MR|D]) :-
  printLine([TL, TM, TR]),
  write("\n"),
  printLine([ML, MM, MR]),
  write("\n"),
  printLine(D),
  write("\n").

printMiddelSidesLine(R, B, O, G, Drop) :-
  drop(Drop, R, RS),
  drop(Drop, B, BS),
  drop(Drop, O, OS),
  drop(Drop, G, GS),
  take(3, RS, RL),
  take(3, BS, BL),
  take(3, OS, OL),
  take(3, GS, GL),
  printLine(RL),
  write(" "),
  printLine(BL),
  write(" "),
  printLine(OL),
  write(" "),
  printLine(GL),
  write("\n").

printLine([H|T]) :-
  write(H), printLine(T).
printLine([]).

read_line(L, C) :-
  get_char(C),
  (isEOFEOL(C), L = [], !;
  read_line(LL,_),
  [C|LL] = L ).

isEOFEOL(C) :-
  C == end_of_file;
  (char_code(C, Code), Code==10).

read_lines(Ls) :-
  read_line(L, C),
  (C == end_of_file, Ls=[] ;
  (read_lines(LLs), [L|LLs] = Ls)).

take(_, [], []).
take(N, [H|T], [H|TT]) :-
  N > 0, !, NN is N-1, take(NN, T, TT).
take(_, _, []).

drop(_, [], []).
drop(N, [_|T], TT) :-
  N > 0, !, NN is N-1, drop(NN, T, TT).
drop(N, [H|T], [H|TT]) :-
  drop(N, T, TT).
