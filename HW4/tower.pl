% Question 1 - tower solution

tower(N, T, C) :-
    % array size limit
    len_row(T, N),
    len_col(T, N),
    counts(U,D,L,R) = C,
    % finish domain limits
    within_domain(T, N),
    maplist(fd_all_different, T),
    transpose(T, Transposed),
    maplist(fd_all_different, Transposed),
    maplist(fd_labeling, T),
    check_normal(Transposed, U),
    check_normal(T, L),
    check_reverse(T, R),
    check_reverse(Transposed, D).

check_reverse([], []).
check_reverse([HD | TL], [X | REST]) :-
    reverse(HD, REV_HD),
    check_row(REV_HD, X),
    check_reverse(TL, REST).

check_normal([], []).
check_normal([HD | TL], [X | REST]) :-
    check_row(HD, X),
    check_normal(TL, REST).

check_row(ROW, NUM) :- 
    count_view(ROW, RES, 0),
    length(RES, LEN),
    LEN #= NUM.

count_view([], [], _).
count_view([HD | TL], [HD | RES], MIN) :-
    HD #> MIN,
    count_view(TL, RES, HD).
count_view([HD | TL], RES, MIN) :-
    HD #< MIN,
    count_view(TL, RES, MIN).

len_row(T, N) :-
    length(T, N).

len_col([], _).
len_col([HD | TL], N) :-
    length(HD, N),
    len_col(TL, N).

within_domain([], _).
within_domain([HD | TL], N) :-
    fd_domain(HD, 1, N),
    within_domain(TL, N).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

% Question 2 - plain_tower solution

plain_tower(N, T, C) :-
    len_row(T, N),
    within_domain_helper(N, X),
    counts(U,D,L,R) = C,
    check_normal_plain(N, T, L, R, X),
    transpose(T, Transposed),
    check_normal_plain(N, Transposed, U, D, X).

within_domain_helper(N, Domain) :- 
    findall(X, between(1, N, X), Domain).

check_row_unique([], []).
check_row_unique([HD | TL], ENUM) :-
    member(HD, ENUM),
    remove(HD, ENUM, ENUM_R),
    check_row_unique(TL, ENUM_R).

remove(X, [X|T], T).
remove(X, [Y|T], [Y|RES]) :-
    remove(X, T, RES).

check_normal_plain(_, [], [], [], _).
check_normal_plain(N, [HD | TL], [X | REST], [R_X | R_REST], Uniq) :-
    length(HD, N),
    check_row_unique(HD, Uniq),
    check_row_plain(HD, X),
    reverse(HD, REV_HD),
    check_row_plain(REV_HD, R_X),
    check_normal_plain(N, TL, REST, R_REST, Uniq).

check_row_plain(ROW, NUM) :- 
    count_view_plain(ROW, 0, 0, NUM).

count_view_plain([], LEN, _, RES) :- RES is LEN.
count_view_plain([HD | TL], LEN, MIN, RES) :-
    HD > MIN,
    X is LEN + 1, 
    count_view_plain(TL, X, HD, RES).
count_view_plain([HD | TL], LEN, MIN, RES) :-
    HD < MIN,
    count_view_plain(TL, LEN, MIN, RES).

% Test Cases

tower_time(Time) :- 
    statistics(cpu_time, [Begin|_]),
    tower(5, _, counts([2,2,4,2,1],[3,2,1,2,5],[3,3,1,2,2],[1,2,3,3,3])),
    statistics(cpu_time, [End|_]),
    Time is (End - Begin).

plain_tower_time(Time) :- 
    statistics(cpu_time, [Begin|_]),
    plain_tower(5, _, counts([2,2,4,2,1],[3,2,1,2,5],[3,3,1,2,2],[1,2,3,3,3])),
    statistics(cpu_time, [End|_]),
    Time is (End - Begin).

% [[3,2,1,2,4],[2,3,3,3,1],[3,4,2,1,2],[3,2,2,4,1]]
% [2,2,4,2,1],[3,2,1,2,5],[3,3,1,2,2],[1,2,3,3,3]

speedup(Res) :-
    tower_time(T_time),
    plain_tower_time(P_time),
    Res is P_time / T_time.

% Ambiguous Tower

ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.