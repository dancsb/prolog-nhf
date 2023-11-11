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

all_the_sator_szukites(_, 0, ILs, ILs).

all_the_sator_szukites(Fs, I, ILs0, ILs) :-
    (sator_szukites(Fs, I, ILs0, Temp) -> NewILs = Temp; NewILs = ILs0),
    NewI is I - 1,
    all_the_sator_szukites(Fs, NewI, NewILs, ILs).    

all_the_sor_szukites(_, _, [], ILs, ILs).

all_the_sor_szukites(Fs, I, [Db | RestDb], ILs0, ILs) :-
    (Db >= 0, osszeg_szukites(Fs, sor(I, Db), ILs0, Temp) -> NewILs = Temp; NewILs = ILs0),
    NewI is I + 1,
    all_the_sor_szukites(Fs, NewI, RestDb, NewILs, ILs).

all_the_oszl_szukites(_, _, [], ILs, ILs).

all_the_oszl_szukites(Fs, I, [Db | RestDb], ILs0, ILs) :-
    (Db >= 0, osszeg_szukites(Fs, oszl(I, Db), ILs0, Temp) -> NewILs = Temp; NewILs = ILs0),
    NewI is I + 1,
    all_the_oszl_szukites(Fs, NewI, RestDb, NewILs, ILs).

do_all_the_things(In, ILs0, ILs) :-
    satrak(Ss, Os, Fs) = In,
    length(Fs, Fsl),   
    all_the_sator_szukites(Fs, Fsl, ILs0, ILs1),
    all_the_sor_szukites(Fs, 1, Ss, ILs1, ILs2),
    all_the_oszl_szukites(Fs, 1, Os, ILs2, ILs).
    
flatten([], []).

flatten([[Dir | _] | RestILs0], [Dir | ILs]) :-
    flatten(RestILs0, ILs).
    
do_the_thing(In, ILs0, ILs) :-
    do_all_the_things(In, ILs0, ILs1),
    (ILs1 = ILs0 -> ILs = ILs1; do_the_thing(In, ILs1, ILs)).

check_megoldas([]).

check_megoldas([ILs | RestILs]) :-
    proper_length(ILs, 1),
    check_megoldas(RestILs).

first_long_sublist(ILs, ForkPoint) :-
    member(ForkPoint, ILs),
    \+proper_length(ForkPoint, 1).

generate_variants(List, ForkPoint, Variants) :-
    append(Before, [ForkPoint|After], List),
    findall(Variant, (member(Elem, ForkPoint), Variant0=[[Elem] | After], append(Before, Variant0, Variant)), Variants).

fork(ILs, Variants) :-
    first_long_sublist(ILs, ForkPoint),
    generate_variants(ILs, ForkPoint, Variants).

solve(In, ILs0, Sols) :-
    do_the_thing(In, ILs0, ILs),
    (check_megoldas(ILs) -> flatten(ILs, Sols);
        fork(ILs, Forks),
        findall(Sol, (member(Fork, Forks), solve(In, Fork, Sol), Sol \= []), Sols)
    ),
    !.

flatten2([], []).

flatten2([L|Ls], FlatL) :-
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).

flatten2(L, [L]).

split_list([], _, []).

split_list(List, Size, [Chunk|Chunks]) :-
    length(Chunk, Size),
    append(Chunk, Rest, List),
    split_list(Rest, Size, Chunks).

satrak_core(In, Out) :-
    satrak(Ss, Os, Fs) = In,
    length(Ss, N),
    length(Os, M),
    length(Fs, Fsl),
    iranylistak(N-M, Fs, ILs0),
    solve(In, ILs0, ILs),
    flatten2(ILs, Out0),
    split_list(Out0, Fsl, Out),
    !.

satrak(In, Out) :-
    satrak_core(In, Out0),
    member(Out, Out0).