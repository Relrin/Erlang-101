-module(champ_stat).

-export([get_stat/1]).

-include_lib("eunit/include/eunit.hrl").


get_team_stat({team, _, Players}) ->
    {TeamSumAge, TeamSumRating} = lists:foldl(
        fun({player, _, Age, Rating, _}, {SumAge, SumRating}) ->
            {SumAge + Age, SumRating + Rating}
        end,
        {0, 0}, Players
    ),
    {length(Players), TeamSumAge, TeamSumRating}.


get_stat(Champ) ->
    TeamsStats = lists:map(fun get_team_stat/1, Champ),
    {NumPlayers, SumAge, SumRating} = lists:foldl(
        fun({TeamSize, TeamSumAge, TeamSumRating}, {NumPlayers, SumAge, SumRating}) ->
            {NumPlayers + TeamSize, SumAge + TeamSumAge, SumRating + TeamSumRating}
        end,
        {0, 0, 0}, TeamsStats
    ),
    {length(Champ), NumPlayers, SumAge / NumPlayers, SumRating / NumPlayers}.


get_stat_test() ->
    ?assertEqual({5,40,24.85,242.8}, get_stat(champ:sample_champ())),
    ok.
