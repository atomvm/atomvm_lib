%%
%% Copyright (c) dushin.net
%% All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
-module(rational).

-export([add/2, subtract/2, multiply/2, divide/2, normalize/1, simplify/1, reduce/1, to_decimal/2, round/1]).

-type numerator() :: integer().
-type denominator() :: integer().
-type fraction() :: {numerator(), denominator()}.
-type composite() :: {integer(), fraction()}.
-type rational() :: integer() | fraction() | composite().

%%
%% @param A rational
%% @param B rational
%% @return A + B as a (possibly top-heavy) fraction.
%%
-spec add(A::rational(), B::rational()) -> fraction().
add(A, B) ->
    add_rational(normalize(A), normalize(B)).

%% @private
add_rational({N1, D1}, {N2, D2}) ->
    {N1 * D2 + N2 * D1, D1 * D2}.

%%
%% @param A rational
%% @param B rational
%% @return A - B as a (possibly top-heavy) fraction.
%%
subtract(A, B) ->
    subtract_rational(normalize(A), normalize(B)).

%% @private
subtract_rational({N1, D1}, {N2, D2}) ->
    {N1 * D2 - N2 * D1, D1 * D2}.

%%
%% @param A rational
%% @param B rational
%% @return A * B as a (possibly top-heavy) fraction.
%%
multiply(A, B) ->
    multiply_rational(normalize(A), normalize(B)).

%% @private
multiply_rational({N1, D1}, {N2, D2}) ->
    {N1 * N2, D1 * D2}.

%%
%% @param A rational
%% @param B rational
%% @return A / B as a (possibly top-heavy) fraction.
%%
divide(A, B) ->
    divide_rational(normalize(A), normalize(B)).

%% @private
divide_rational({0, _}, _) ->
    0;
divide_rational(_, {0, _}) ->
    undefined;
divide_rational({N1, D1}, {N2, D2}) ->
    {N1 * D2, D1 * N2}.

%%
%% @param R rational
%% @return (possibly top-heavy) fraction.
%%
-spec normalize(R::rational()) -> fraction().
normalize(X) when is_integer(X) -> {X, 1};
normalize({I, {N, D}}) when is_integer(I) andalso is_integer(N) andalso is_integer(D) ->
    {I * D + N, D};
normalize({N, D} = R) when is_integer(N) andalso is_integer(D) ->
    R.

%%
%% @param X (possibly top-heavy) fraction
%% @return simplified rational, a fraction, integer or composite.
%%
-spec simplify(F::fraction()) -> rational().
% simplify({N, 1}) ->
%     N;
% simplify({N, N}) ->
%     1;
% simplify({0, D}) ->
%     0;
% simplify({_N, 0}) ->
%     undefined;
simplify({N, D}) when is_integer(D) andalso N > D ->
    case N rem D of
        0 ->
            {N div D, {0, D}};
        R ->
            {N div D, reduce({R, D})}
    end;
simplify(X) ->
    X.

%%
%% @param X (possibly top-heavy) fraction
%% @return reduced fraction (possibly divided by gcd).
%%
-spec reduce(F::fraction()) -> fraction().
reduce({N, D}) when is_integer(N) andalso is_integer(D) ->
    case gcd(N, D) of
        1 ->
            {N, D};
        G ->
            {N div G, D div G}
    end.

%% @private
gcd(A, B) when B > A ->
    gcd(B, A);
gcd(A, B) ->
    case A rem B of
        0 -> B;
        R ->
            gcd(B, R)
    end.

%%
%% @param F (possibly top-heavy) fraction
%% @param P desired precision
%% @return equivalent fraction whose denominator is 10^{Precision}.
%% @doc Note that no rounding is performed on the last digit,
%% and that in general the returned fraction is an estimate.
%%
-spec to_decimal(F::fraction(), P::non_neg_integer()) -> rational().
to_decimal(N, Precision) when is_integer(N) ->
    {N, {0, pow(10, Precision)}};
to_decimal({N, D}, Precision) when is_integer(N) andalso is_integer(D) ->
    case simplify({N, D}) of
        {I, {N1, D1}} ->
            {I, to_decimal({N1, D1}, Precision)};
        {N1, D1} ->
            Digits = long_division(D1, N1, Precision, []),
            {to_number(Digits), pow(10, Precision)};
        undefined ->
            undefined;
        N ->
            {N, {0, 10}}
    end.

%% @private
long_division(_D, _N1, 0, Accum) ->
    Accum;
long_division(D, N1, Precision, Accum) ->
    N2 = N1 * 10,
    X = N2 div D,
    long_division(D, N2 - X * D, Precision - 1, [X | Accum]).

%%
%% @param R rational
%% @return Rounded off integer
%%
-spec round(R::rational()) -> integer().
round({I, {N, D} = F}) when is_integer(I) andalso is_integer(N) andalso is_integer(D) ->
    I + ?MODULE:round(F);
round({N, D}) when is_integer(N) andalso is_integer(D) ->
    case N < (D bsr 1) of
        true -> 0;
        _ -> 1
    end;
round(I) when is_integer(I) ->
    I.

%% @private
to_number(Digits) ->
    to_number(Digits, 1, 0).

%% @private
to_number([], _I, Accum) ->
    Accum;
to_number([D|T], I, Accum) ->
    to_number(T, I*10, D * I + Accum).

%% @private
pow(_B, 0) ->
    1;
pow(B, N) ->
    B * pow(B, N - 1).
