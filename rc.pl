:- use_module(library(lists)).


:- initialization main.


main :-
  read_lines(L),
  parseCube(L, C),
  SC =.. [solvedCube,['5','5','5','5','5','5','5','5','5'],  %White
                     ['1','1','1','1','1','1','1','1','1'],  %Red
                     ['2','2','2','2','2','2','2','2','2'],  %Blue
                     ['3','3','3','3','3','3','3','3','3'],  %Orange
                     ['4','4','4','4','4','4','4','4','4'],  %Green
                     ['6','6','6','6','6','6','6','6','6']], %Yellow
  !,
  assertz(SC),
  assertz(node(C, root, nonexpanded)),
  solveCube,
  !,
  node(C, _, solved),
  backtrackShortestPath(C, Path),
  !,
  reverse(Path, R),
  printSolution(R).

solveCube :-
  node(cube(W, R, B, O, G, Y), P, nonexpanded),
  solvedCube(W, R, B, O, G, Y),
  retract(node(cube(W, R, B, O, G, Y), P, nonexpanded)),
  assertz(node(cube(W, R, B, O, G, Y), P, solved)).
/*
solveCube :-
  assertz(C),
  rotate(C, cube(RW, RR, RB, RO, RG, RY)),
  not(cube(RW, RR, RB, RO, RG, RY)),
  solveCube(cube(RW, RR, RB, RO, RG, RY), SC).
solveCube(cube(W, R, B, O, G, Y), _) :-
  cube(W, R, B, O, G, Y),
  retract(cube(W, R, B, O, G, Y)),
  fail.

rotate(C, RC) :-
  rotateWhite(C, RC).
rotate(C, RC) :-
  rotateRed(C, RC).
rotate(C, RC) :-
  rotateBlue(C, RC).
rotate(C, RC) :-
  rotateOrange(C, RC).
rotate(C, RC) :-
  rotateGreen(C, RC).
rotate(C, RC) :-
  rotateYellow(C, RC).
*/

backtrackShortestPath(root, _).
backtrackShortestPath(P, [P|T]) :-
  node(P, NP, _),
  backtrackShortestPath(NP, T).

printSolution([]).
printSolution([H|T]) :-
  printCube(H),
  printSolution(T).

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

rotateRed(cube([WTL, WTM, WTR, WML, WMM, WMR, WDL, WDM, WDR],
               [RTL, RTM, RTR, RML, RMM, RMR, RDL, RDM, RDR],
               [BTL, BTM, BTR, BML, BMM, BMR, BDL, BDM, BDR],
                O,
               [GTL, GTM, GTR, GML, GMM, GMR, GDL, GDM, GDR],
               [YTL, YTM, YTR|YT]), RC) :-
  W = [WTL, WTM, WTR, WML, WMM, WMR, GDR, GMR, GTR],
  R = [RDL, RML, RTL, RDM, RMM, RTM, RDR, RMR, RTR],
  B = [WDL, BTM, BTR, WDM, BMM, BMR, WDR, BDM, BDR],
  G = [GTL, GTM, YTL, GML, GMM, YTM, GDL, GDM, YTR],
  Y = [BDL, BML, BTL|YT],
  RC =.. [cube, W, R, B, O, G, Y].

rotateBlue(cube([WTL, WTM, WTR, WML, WMM, WMR, WDL, WDM, WDR],
                [RTL, RTM, RTR, RML, RMM, RMR, RDL, RDM, RDR],
                [BTL, BTM, BTR, BML, BMM, BMR, BDL, BDM, BDR],
                [OTL, OTM, OTR, OML, OMM, OMR, ODL, ODM, ODR],
                 G,
                [YTL, YTM, YTR, YML, YMM, YMR, YDL, YDM, YDR]), RC) :-
W = [WTL, WTM, RTR, WML, WMM, RMR, WDL, WDM, RDR],
R = [RTL, RTM, YTR, RML, RMM, YMR, RDL, RDM, YDR],
B = [BDL, BML, BTL, BDM, BMM, BTM, BDR, BMR, BTR],
O = [WDR, OTM, OTR, WMR, OMM, OMR, WTR, ODM, ODR],
Y = [YTL, YTM, ODL, YML, YMM, OML, YDL, YDM, OTL],
RC =.. [cube, W, R, B, O, G, Y].

rotateOrange(cube([WTL, WTM, WTR|WT],
                   R,
                  [BTL, BTM, BTR, BML, BMM, BMR, BDL, BDM, BDR],
                  [OTL, OTM, OTR, OML, OMM, OMR, ODL, ODM, ODR],
                  [GTL, GTM, GTR, GML, GMM, GMR, GDL, GDM, GDR],
                  [YTL, YTM, YTR, YML, YMM, YMR, YDL, YDM, YDR]), RC) :-
W = [BTR, BMR, BDR|WT],
B = [BTL, BTM, YDR, BML, BMM, YDM, BDL, BDM, YDL],
O = [ODL, OML, OTL, ODM, OMM, OTM, ODR, OMR, OTR],
G = [WTR, GTM, GTR, WTM, GMM, GMR, WTL, GDM, GDR],
Y = [YTL, YTM, YTR, YML, YMM, YMR, GTL, GML, GDL],
RC =.. [cube, W, R, B, O, G, Y].

rotateGreen(cube([WTL, WTM, WTR, WML, WMM, WMR, WDL, WDM, WDR],
                  [RTL, RTM, RTR, RML, RMM, RMR, RDL, RDM, RDR],
                   B,
                  [OTL, OTM, OTR, OML, OMM, OMR, ODL, ODM, ODR],
                  [GTL, GTM, GTR, GML, GMM, GMR, GDL, GDM, GDR],
                  [YTL, YTM, YTR, YML, YMM, YMR, YDL, YDM, YDR]), RC) :-
W = [ODR, WTM, WTR, OMR, WMM, WMR, OTR, WDM, WDR],
R = [WTL, RTM, RTR, WML, RMM, RMR, WDL, RDM, RDR],
O = [OTL, OTM, YDL, OML, OMM, YML, ODL, ODM, YTL],
G = [GDL, GML, GTL, GDM, GMM, GTM, GDR, GMR, GTR],
Y = [RTL, YTM, YTR, RML, YMM, YMR, RDL, YDM, YDR],
RC =.. [cube, W, R, B, O, G, Y].

rotateYellow(cube( W,
                  [RTL, RTM, RTR, RML, RMM, RMR, RDL, RDM, RDR],
                  [BTL, BTM, BTR, BML, BMM, BMR, BDL, BDM, BDR],
                  [OTL, OTM, OTR, OML, OMM, OMR, ODL, ODM, ODR],
                  [GTL, GTM, GTR, GML, GMM, GMR, GDL, GDM, GDR],
                  [YTL, YTM, YTR, YML, YMM, YMR, YDL, YDM, YDR]), RC) :-
R = [RTL, RTM, RTR, RML, RMM, RMR, GDL, GDM, GDR],
B = [BTL, BTM, BTR, BML, BMM, BMR, RDL, RDM, RDR],
O = [OTL, OTM, OTR, OML, OMM, OMR, BDL, BDM, BDR],
G = [GTL, GTM, GTR, GML, GMM, GMR, ODL, ODM, ODR],
Y = [YDL, YML, YTL, YDM, YMM, YTM, YDR, YMR, YTR],
RC =.. [cube, W, R, B, O, G, Y].

parseCube(L, C) :-
  parseWhiteSide(L, W),
  parseMiddleSide(L, 0, R),
  parseMiddleSide(L, 4, B),
  parseMiddleSide(L, 8, O),
  parseMiddleSide(L, 12, G),
  parseYellowSide(L, Y),
  C =.. [cube, W, R, B, O, G, Y].

printCube(cube(W, R, B, O, G, Y)) :-
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
