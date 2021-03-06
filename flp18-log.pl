/*
  project: Rubikova kostka
  login: xkohut08
  name: Jan Kohút
  date: 29. 4. 2018
*/
:- use_module(library(lists)).

:- initialization(main, restore).


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
  assertz(node(C, root, nonexpanded, none)),
  assertz(SC),
  solveCube,
  !,
  node(FC, _, solved, _),
  backtrackShortestPath(FC, Path),
  !,
  reverse(Path, R),
  printSolution(R),
  halt.

main :-
  halt(1).

% Breadth-first search algorithm

% Check if any nonexpanded cube in database is solved
solveCube :-
  node(cube(W, R, B, O, G, Y), P, nonexpanded, none),
  retract(node(cube(W, R, B, O, G, Y), P, nonexpanded, none)),
  assertz(node(cube(W, R, B, O, G, Y), P, nonexpanded, compared)),
  solvedCube(W, R, B, O, G, Y),
  retract(node(cube(W, R, B, O, G, Y), P, nonexpanded, compared)),
  assertz(node(cube(W, R, B, O, G, Y), P, solved, compared)).

% Expand first nonexpanded cube in database
solveCube :-
  node(C, P, nonexpanded, compared),
  % Rotate cube in every possible way
  rotateWhite(C, W90),
  rotateRed(C, R90),
  rotateBlue(C, B90),
  rotateOrange(C, O90),
  rotateGreen(C, G90),
  rotateYellow(C, Y90),
  rotateWhite(W90, W180),
  rotateRed(R90, R180),
  rotateBlue(B90, B180),
  rotateOrange(O90, O180),
  rotateGreen(G90, G180),
  rotateYellow(Y90, Y180),
  rotateWhite(W180, W270),
  rotateRed(R180, R270),
  rotateBlue(B180, B270),
  rotateOrange(O180, O270),
  rotateGreen(G180, G270),
  rotateYellow(Y180, Y270),
  % Add rotated cubes to database (ignore 180)
  addCube(W90, C),
  addCube(R90, C),
  addCube(B90, C),
  addCube(O90, C),
  addCube(G90, C),
  addCube(Y90, C),
  addCube(W270, C),
  addCube(R270, C),
  addCube(B270, C),
  addCube(O270, C),
  addCube(G270, C),
  addCube(Y270, C),
  retract(node(C, P, nonexpanded, compared)),
  assertz(node(C, P, expanded, compared)),
  solveCube.

% Add cube to database
addCube(C, P) :-
  % Check if cube is already in database
  not(node(C, P, _, _)),
  assertz(node(C, P, nonexpanded, none)).
addCube(_,_).

% Backtrack path from solved cube to root cube using parents
backtrackShortestPath(root, _).
backtrackShortestPath(P, [P|T]) :-
  node(P, NP, _, _),
  backtrackShortestPath(NP, T).

printSolution([]).
printSolution([H]) :-
  printCube(H).
printSolution([H|T]) :-
  printCube(H),
  write('\n'),
  printSolution(T).

% Rotation for every side of cube
% 5 - White
% 1 - Red
% 2 - Blue
% 3 - Orange
% 4 - Green
% 6 - Yellow
% T - Top
% M - Middle
% D - Down
% L - Left
% R - Right

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

% Convert input to cube representation
parseCube(L, C) :-
  parseWhiteSide(L, W),
  parseMiddleSide(L, 0, R),
  parseMiddleSide(L, 4, B),
  parseMiddleSide(L, 8, O),
  parseMiddleSide(L, 12, G),
  parseYellowSide(L, Y),
  C =.. [cube, W, R, B, O, G, Y].

% Convert cube representation to output
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
  write('\n'),
  printLine([ML, MM, MR]),
  write('\n'),
  printLine(D),
  write('\n').

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
  write(' '),
  printLine(BL),
  write(' '),
  printLine(OL),
  write(' '),
  printLine(GL),
  write('\n').

printLine([H|T]) :-
  write(H), printLine(T).
printLine([]).

take(_, [], []).
take(N, [H|T], [H|TT]) :-
  N > 0, !, NN is N-1, take(NN, T, TT).
take(_, _, []).

drop(_, [], []).
drop(N, [_|T], TT) :-
  N > 0, !, NN is N-1, drop(NN, T, TT).
drop(N, [H|T], [H|TT]) :-
  drop(N, T, TT).

% Implementation of input reading from labs (cv4)
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
