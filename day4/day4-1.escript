#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Day 4: Passport Processing ---

%% You arrive at the airport only to realize that you grabbed your
%% North Pole Credentials instead of your passport. While these
%% documents are extremely similar, North Pole Credentials aren't
%% issued by a country and therefore aren't actually valid
%% documentation for travel in most of the world.

%% It seems like you're not the only one having problems, though; a
%% very long line has formed for the automatic passport scanners, and
%% the delay could upset your travel itinerary.

%% Due to some questionable network security, you realize you might be
%% able to solve both of these problems at the same time.

%% The automatic passport scanners are slow because they're having
%% trouble detecting which passports have all required fields. The
%% expected fields are as follows:

%%     byr (Birth Year)
%%     iyr (Issue Year)
%%     eyr (Expiration Year)
%%     hgt (Height)
%%     hcl (Hair Color)
%%     ecl (Eye Color)
%%     pid (Passport ID)
%%     cid (Country ID)

%% Passport data is validated in batch files (your puzzle input). Each
%% passport is represented as a sequence of key:value pairs separated
%% by spaces or newlines. Passports are separated by blank lines.

%% Here is an example batch file containing four passports:

%% ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
%% byr:1937 iyr:2017 cid:147 hgt:183cm

%% iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
%% hcl:#cfa07d byr:1929

%% hcl:#ae17e1 iyr:2013
%% eyr:2024
%% ecl:brn pid:760753108 byr:1931
%% hgt:179cm

%% hcl:#cfa07d eyr:2025 pid:166559648
%% iyr:2011 ecl:brn hgt:59in

%% The first passport is valid - all eight fields are present. The
%% second passport is invalid - it is missing hgt (the Height field).

%% The third passport is interesting; the only missing field is cid, so
%% it looks like data from North Pole Credentials, not a passport at
%% all! Surely, nobody would mind if you made the

%% system temporarily ignore missing cid fields. Treat this "passport"
%% as valid.

%% The fourth passport is missing two fields, cid and byr. Missing cid
%% is fine, but missing any other field is not, so this passport is
%% invalid.

%% According to the above rules, your improved system would report 2
%% valid passports.

%% Count the number of valid passports - those that have all required
%% fields. Treat cid as optional. In your batch file, how many
%% passports are valid?

-define(CodeBirthYear, <<"byr">>).
-define(CodeIssueYear, <<"iyr">>).
-define(CodeExpirationYear, <<"eyr">>).
-define(CodeHeight, <<"hgt">>).
-define(CodeHairColor, <<"hcl">>).
-define(CodeEyeColor, <<"ecl">>).
-define(CodePassportId, <<"pid">>).
-define(CodeCountryId, <<"cid">>).


-record(passport, {birth_year
                  ,issue_year
                  ,expiration_year
                  ,height
                  ,hair_color
                  ,eye_color
                  ,passport_id
                  ,country_id = 0 %% since optional, default to 0
                  }).

main(_) ->
    Passports = read_input("p4.txt"),
    Valid = lists:filter(fun is_valid_passport/1, Passports),
    io:format("valid: ~p~n", [length(Valid)]).

is_valid_passport(Passport) ->
    ['passport' | Fields] = tuple_to_list(Passport),
    lists:all(fun not_undefined/1, Fields).

not_undefined('undefined') -> 'false';
not_undefined(_) -> 'true'.

read_input(File) ->
    {'ok', Map} = file:read_file(File),
    Lines = binary:split(Map, <<"\n">>, ['global']),
    parse_passports(Lines).

parse_passports(Lines) ->
    {Passport, Passports} = lists:foldl(fun parse_passport/2
                                       ,{#passport{}, []}
                                       ,Lines
                                       ),
    case Passport of
        #passport{hair_color='undefined'} -> Passports;
        Passport -> [Passport | Passports]
    end.

parse_passport(<<>>, {Passport, Passports}) ->
    {#passport{}, [Passport | Passports]};
parse_passport(Line, {Passport, Passports}) ->
    {'match', Properties} =
        re:run(Line
              ,<<"(\\w+):([^\\s]+)">>
              ,[{'capture', 'all_but_first', 'binary'}
               ,'global'
               ]),
    {lists:foldl(fun add_property/2, Passport, Properties)
    ,Passports
    }.

add_property([?CodeBirthYear, Year], Passport) ->
    Passport#passport{birth_year=binary_to_integer(Year, 10)};
add_property([?CodeIssueYear, Year], Passport) ->
    Passport#passport{issue_year=binary_to_integer(Year, 10)};
add_property([?CodeExpirationYear, Year], Passport) ->
    Passport#passport{expiration_year=binary_to_integer(Year, 10)};
add_property([?CodeHeight, Height], Passport) ->
    Passport#passport{height=Height};
add_property([?CodeHairColor, Color], Passport) ->
    Passport#passport{hair_color=Color};
add_property([?CodeEyeColor, Color], Passport) ->
    Passport#passport{eye_color=Color};
add_property([?CodePassportId, Id], Passport) ->
    Passport#passport{passport_id=Id};
add_property([?CodeCountryId, Id], Passport) ->
    Passport#passport{country_id=Id}.
