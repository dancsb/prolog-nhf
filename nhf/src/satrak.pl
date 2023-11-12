% :- type fLeiro  ---> satrak(sSzS, sSzO, list(parc)).
% :- type sSzS    == list(int).
% :- type sSzO    == list(int).
% :- type parc    == int-int.
% :- type irany   ---> n    % észak
%                 ;    e    % kelet
%                 ;    s    % dél
%                 ;    w.   % nyugat
% :- type sHelyek   == list(irany).
% :- pred satrak(fLeiro::in, sHelyek::out).
:- use_module(library(lists)). % SICStus lists library betöltése

% Sátor meghatározása fa és irány alapján
get_tent(Fx-Fy, e, Tx-Ty) :- Fy1 is Fy + 1, Tx-Ty = Fx - Fy1. % Kelet
get_tent(Fx-Fy, n, Tx-Ty) :- Fx1 is Fx - 1, Tx-Ty = Fx1 - Fy. % Észak
get_tent(Fx-Fy, s, Tx-Ty) :- Fx1 is Fx + 1, Tx-Ty = Fx1 - Fy. % Dél
get_tent(Fx-Fy, w, Tx-Ty) :- Fy1 is Fy - 1, Tx-Ty = Fx - Fy1. % Nyugat

% Egy adott fa iránylistájának meghatározása. Egy adott irány akkor megfelelő, ha az általa mutatott sátor nem lóg le a pályáról, és nem létezik olyan fa.
get_directions(N-M, Fs, Fx-Fy, Dir) :-
    (get_tent(Fx-Fy, e, Txe-Tye), Tye =< M, nonmember(Txe-Tye, Fs) -> EDir = [e]; EDir = []), % Kelet
    (get_tent(Fx-Fy, n, Txn-Tyn), Txn >= 1, nonmember(Txn-Tyn, Fs) -> append(EDir, [n], NDir); NDir = EDir), % Észak
    (get_tent(Fx-Fy, s, Txs-Tys), Txs =< N, nonmember(Txs-Tys, Fs) -> append(NDir, [s], SDir); SDir = NDir), % Dél
    (get_tent(Fx-Fy, w, Txw-Tyw), Tyw >= 1, nonmember(Txw-Tyw, Fs) -> append(SDir, [w], WDir); WDir = SDir), % Nyugat
    Dir = WDir.

% Elfogytak a fák
iranylistak_core(_, _, [], []).

% Iránylisták meghatározása rekurzívan a fák alapján
iranylistak_core(N-M, Fs, [F | RestFs], [Dir | RestILs]) :-
    get_directions(N-M, Fs, F, Dir),
    iranylistak_core(N-M, Fs, RestFs, RestILs).

iranylistak(N-M, Fs, ILs) :-
    iranylistak_core(N-M, Fs, Fs, Dir), % Iránylisták generálása
    (nonmember([], Dir) -> ILs = Dir; ILs = []), % Üres iránylisták ellenőrzése
    !. % cut

% Adott fa meglévő iránylistájának újrartékelése, szűkítése. Egy adott irány akkor maradhat, ha eddig is volt, és az általa mutatott sátor X ésvagy Y koordinátájának a deltája nagyobb mint 1 az I-edik fa sátorához képest.
reevaluate_directions(Fx-Fy, Ix-Iy, PrevDir, Dir) :-
    (member(e, PrevDir), get_tent(Fx-Fy, e, Txe-Tye), Dxe is Ix - Txe, Dye is Iy - Tye, (abs(Dxe) > 1; abs(Dye) > 1) -> EDir = [e]; EDir = []), % Kelet
    (member(n, PrevDir), get_tent(Fx-Fy, n, Txn-Tyn), Dxn is Ix - Txn, Dyn is Iy - Tyn, (abs(Dxn) > 1; abs(Dyn) > 1) -> append(EDir, [n], NDir); NDir = EDir), % Észak
    (member(s, PrevDir), get_tent(Fx-Fy, s, Txs-Tys), Dxs is Ix - Txs, Dys is Iy - Tys, (abs(Dxs) > 1; abs(Dys) > 1) -> append(NDir, [s], SDir); SDir = NDir), % Dél
    (member(w, PrevDir), get_tent(Fx-Fy, w, Txw-Tyw), Dxw is Ix - Txw, Dyw is Iy - Tyw, (abs(Dxw) > 1; abs(Dyw) > 1) -> append(SDir, [w], WDir); WDir = SDir), % Nyugat
    Dir = WDir.

% Elfogytak az iránylisták
sator_szukites_core([], _, _, [], []).

% Iránylisták szűkítése rekurzívan
sator_szukites_core([F | RestFs], If, Ixy, [Dir | RestILs0], [NewDir | RestILs]) :-
    (If \= F -> reevaluate_directions(F, Ixy, Dir, NewDir); NewDir = Dir), % Szűkítés futtatása, kivéve, ha az I-edik fánál járunk
    sator_szukites_core(RestFs, If, Ixy, RestILs0, RestILs).

sator_szukites(Fs, I, ILs0, ILs) :-
    nth1(I, Fs, If), % I-edik fa meghatározása
    nth1(I, ILs0, Id), % I-edik fa iránylistájának meghatározása
    proper_length(Id, 1), % Ellenőrzése, hogy tényleg csak 1 irányt tartalmaz-e
    [IdH | _] = Id, % I-edik fa sátorának irányának meghatározása
    get_tent(If, IdH, Ixy), % I-edik fa sátorának meghatározása
    sator_szukites_core(Fs, If, Ixy, ILs0, Dir), % Szűkített iránylisták generálása
    (nonmember([], Dir) -> ILs = Dir; ILs = []), % Üres iránylisták ellenőrzése
    !. % cut

% Elfogytak a fák
osszeg_szukites_core([], _, [], [], []).

% A biztosan illetve esetleg egy adott sorba mutató fák meghatározása rekurzívan. A nem megfeleő elemeket üres listák jelölik.
osszeg_szukites_core([Fx-Fy | RestFs], sor(I, _), [Dir | RestILs], [NewB | B], [NewE | E]) :-
    (((Fx = I, subseq0([e,w], Dir)); (Fx is I - 1, Dir = [s]); (Fx is I + 1, Dir = [n])) -> NewB = Fx-Fy, NewE = [];
        (((Fx = I, (member(e, Dir); member(w, Dir))); (Fx is I - 1, member(s, Dir)); (Fx is I + 1, member(n, Dir))) -> NewE = Fx-Fy; NewE = []),
        NewB = []
    ),
    osszeg_szukites_core(RestFs, sor(I, _), RestILs, B, E).

% A biztosan illetve esetleg egy adott oszlopba mutató fák meghatározása rekurzívan. A nem megfeleő elemeket üres listák jelölik.
osszeg_szukites_core([Fx-Fy | RestFs], oszl(J, _), [Dir | RestILs], [NewB | B], [NewE | E]) :-
    (((Fy = J, subseq0([n,s], Dir)); (Fy is J - 1, Dir = [e]); (Fy is J + 1, Dir = [w])) -> NewB = Fx-Fy, NewE = [];
        (((Fy = J, (member(n, Dir); member(s, Dir))); (Fy is J - 1, member(e, Dir)); (Fy is J + 1, member(w, Dir))) -> NewE = Fx-Fy; NewE = []),
        NewB = []
    ),
    osszeg_szukites_core(RestFs, oszl(J, _), RestILs, B, E).

% Adott sorba esetlegesen mutató fa meglévő iránylistájának újrartékelése, szűkítése. Egy adott irány akkor maradhat, ha eddig is volt, és az ii. vagy iii. eset szerint maradnia kell.
filter_ils_core(Fx-_, sor(I, _), Dir, Case, NewDir) :-
    (member(e, Dir), ((Fx = I, Case = ii); (Fx \= I, Case = iii)) -> EDir = [e]; EDir = []), % Kelet
    (member(n, Dir), ((Fx is I + 1, Case = ii); (\+Fx is I + 1, Case = iii)) -> append(EDir, [n], NDir); NDir = EDir), % Észak
    (member(s, Dir), ((Fx is I - 1, Case = ii); (\+Fx is I - 1, Case = iii)) -> append(NDir, [s], SDir); SDir = NDir), % Dél
    (member(w, Dir), ((Fx = I, Case = ii); (Fx \= I, Case = iii)) -> append(SDir, [w], WDir); WDir = SDir), % Nyugat
    NewDir = WDir.

% Adott oszlopba esetlegesen mutató fa meglévő iránylistájának újrartékelése, szűkítése. Egy adott irány akkor maradhat, ha eddig is volt, és az ii. vagy iii. eset szerint maradnia kell.
filter_ils_core(_-Fy, oszl(J, _), Dir, Case, NewDir) :-
    (member(e, Dir), ((Fy is J - 1, Case = ii); (\+Fy is J - 1, Case = iii)) -> EDir = [e]; EDir = []), % Kelet
    (member(n, Dir), ((Fy = J, Case = ii); (Fy \= J, Case = iii)) -> append(EDir, [n], NDir); NDir = EDir), % Észak
    (member(s, Dir), ((Fy = J, Case = ii); (Fy \= J, Case = iii)) -> append(NDir, [s], SDir); SDir = NDir), % Dél
    (member(w, Dir), ((Fy is J + 1, Case = ii); (\+Fy is J + 1, Case = iii)) -> append(SDir, [w], WDir); WDir = SDir), % Nyugat
    NewDir = WDir.

% Elfogytak a fák
filter_ils([], _, [], _, _, []).

% Iránylisták szűkítése rekurzívan
filter_ils([Fs | RestFs], Osszegfeltetel, [Dir | RestILs0], E, Case, [NewDir | RestILs]) :-
    (member(Fs, E) -> filter_ils_core(Fs, Osszegfeltetel, Dir, Case, NewDir); NewDir = Dir),
    filter_ils(RestFs, Osszegfeltetel, RestILs0, E, Case, RestILs).

filter(X) :- X = []. % Üres listák szűrése

osszeg_szukites(Fs, Osszegfeltetel, ILs0, ILs) :-
    osszeg_szukites_core(Fs, Osszegfeltetel, ILs0, B0, E0), % A biztosan illetve esetleg egy adott sorba/oszlopba mutató fák kigyűjtése
    exclude(filter, B0, B), length(B, Bl), % A nem megfelelő elemek szűrése, a biztosan egy adott sorba/oszlopba mutató fák megszámlálása
    exclude(filter, E0, E), length(E, El), % A nem megfelelő elemek szűrése, az esetleg egy adott sorba/oszlopba mutató fák megszámlálása
    BEl is Bl + El, % b + e számítása
    arg(2, Osszegfeltetel, Db), % sor/oszlop darabszám meghatározása
    ((BEl < Db; Bl > Db) -> ILs = []; % i. vagy iv. eset
        (BEl = Db -> filter_ils(Fs, Osszegfeltetel, ILs0, E, ii, ILs); % ii. eset
            (Bl = Db -> filter_ils(Fs, Osszegfeltetel, ILs0, E, iii, ILs); % iii. eset
                fail % Egyik sem
            )
        )
    ),
    !. % cut

% Elfogytak a fák
every_sator_szukites(_, 0, ILs, ILs).

% Összes fára sátorszűkítés alkalmazása rekurzívan
every_sator_szukites(Fs, I, ILs0, ILs) :-
    (sator_szukites(Fs, I, ILs0, Temp) -> NewILs = Temp; NewILs = ILs0),
    NewI is I - 1,
    every_sator_szukites(Fs, NewI, NewILs, ILs).    

% Elfogytak a sorok
every_sor_szukites(_, _, [], ILs, ILs).

% Összes sorra összegszűkítés alkalmazása rekurzívan
every_sor_szukites(Fs, I, [Db | RestDb], ILs0, ILs) :-
    (Db >= 0, osszeg_szukites(Fs, sor(I, Db), ILs0, Temp) -> NewILs = Temp; NewILs = ILs0),
    NewI is I + 1,
    every_sor_szukites(Fs, NewI, RestDb, NewILs, ILs).

% Elfogytak az oszlopok
every_oszl_szukites(_, _, [], ILs, ILs).

% Összes oszlopra összegszűkítés alkalmazása rekurzívan
every_oszl_szukites(Fs, I, [Db | RestDb], ILs0, ILs) :-
    (Db >= 0, osszeg_szukites(Fs, oszl(I, Db), ILs0, Temp) -> NewILs = Temp; NewILs = ILs0),
    NewI is I + 1,
    every_oszl_szukites(Fs, NewI, RestDb, NewILs, ILs).

% Összes fajta szűkítés alkalmazása, amíg csak lehet rekurzívan
every_szukites(In, ILs0, ILs) :-
    satrak(Ss, Os, Fs) = In,
    length(Fs, Fsl),   
    every_sator_szukites(Fs, Fsl, ILs0, ILs1),
    every_sor_szukites(Fs, 1, Ss, ILs1, ILs2),
    every_oszl_szukites(Fs, 1, Os, ILs2, ILs3),
    (ILs3 = ILs0 -> ILs = ILs3; every_szukites(In, ILs3, ILs)).

% Elfogytak az iránylisták
check_solution([]).

% Iránylisták ellenőrzése, hogy mindegyik egyértelmű-e
check_solution([ILs | RestILs]) :-
    proper_length(ILs, 1),
    check_solution(RestILs).

% Elfogytak az iránylisták
flatten([], []).

% Egyértelmű iránylistákból az irányok összefűzése egy listába
flatten([[Dir | _] | RestILs0], [Dir | ILs]) :-
    flatten(RestILs0, ILs).

% Első lehetsétges elágazási pont megkeresése (első nem egyértelmű iránylista), és elágazások generálása
fork(ILs, Forks) :-
    member(ForkPoint, ILs),
    \+proper_length(ForkPoint, 1),
    append(Before, [ForkPoint | After], ILs),
    findall(Fork, (member(Dir, ForkPoint), Fork0=[[Dir] | After], append(Before, Fork0, Fork)), Forks).

% Megoldások generálása
solve(In, ILs0, Sols) :-
    every_szukites(In, ILs0, ILs),
    (check_solution(ILs) -> flatten(ILs, Sols);
        fork(ILs, Forks),
        findall(Sol, (member(Fork, Forks), solve(In, Fork, Sol), Sol \= []), Sols)
    ),
    !. % cut

flatten2([], []).

% Lista kilapítása
flatten2([List | RestList], FlatList) :-
    flatten2(List, NewList),
    flatten2(RestList, NewRestList),
    append(NewList, NewRestList, FlatList),
    !. % cut

flatten2(List, [List]).

% Elfogytak a megoldásaok
split_sols([], _, []).

% Megoldások szétválasztása a kilapított listából
split_sols(Sols0, Size, [Sol | Sols]) :-
    length(Sol, Size),
    append(Sol, RestSols0, Sols0),
    split_sols(RestSols0, Size, Sols).

satrak(In, Out) :-
    satrak(Ss, Os, Fs) = In,
    length(Ss, N),
    length(Os, M),
    length(Fs, Fsl),
    iranylistak(N-M, Fs, ILs),
    solve(In, ILs, Sols0),
    flatten2(Sols0, Sols),
    split_sols(Sols, Fsl, Out0),
    member(Out, Out0).