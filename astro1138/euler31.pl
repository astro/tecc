
a(0, _).
a(N, M) :- M > 0, M2 is M - 1, a(N2, M2), N is N2 + 1.

coins([A1, A2, A5, A10, A20, A50, A100, A200]) :-
	a(A1, 200), a(A2, 100), a(A5, 40), a(A10, 20), a(A20, 10), a(A50, 4), a(A100, 2), a(A200, 1),
	A1 * 1 + A2 * 2 + A5 * 5 + A10 * 10 + A20 * 20 + A50 * 50 + A100 * 100 + A200 * 200 is 200.

euler31(NWays) :-
	findall(Coins, coins(Coins), Ways),
	length(Ways, NWays).

