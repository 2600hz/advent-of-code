#!/usr/bin/env escript
%%! -sname advent
%% -*- coding: utf-8 -*-

-mode(compile).

%% --- Part Two ---

%% The line is moving more quickly now, but you overhear airport
%% security talking about how passports with invalid data are getting
%% through. Better add some data validation, quick!

%% You can continue to ignore the cid field, but each other field has
%% strict rules about what values are valid for automatic validation:

%%     byr (Birth Year) - four digits; at least 1920 and at most 2002.
%%     iyr (Issue Year) - four digits; at least 2010 and at most 2020.
%%     eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
%%     hgt (Height) - a number followed by either cm or in:
%%         If cm, the number must be at least 150 and at most 193.
%%         If in, the number must be at least 59 and at most 76.
%%     hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
%%     ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
%%     pid (Passport ID) - a nine-digit number, including leading zeroes.
%%     cid (Country ID) - ignored, missing or not.

%% Your job is to count the passports where all required fields are
%% both present and valid according to the above rules. Here are some
%% example values:

%% byr valid:   2002
%% byr invalid: 2003

%% hgt valid:   60in
%% hgt valid:   190cm
%% hgt invalid: 190in
%% hgt invalid: 190

%% hcl valid:   #123abc
%% hcl invalid: #123abz
%% hcl invalid: 123abc

%% ecl valid:   brn
%% ecl invalid: wat

%% pid valid:   000000001
%% pid invalid: 0123456789

%% Here are some invalid passports:

%% eyr:1972 cid:100
%% hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

%% iyr:2019
%% hcl:#602927 eyr:1967 hgt:170cm
%% ecl:grn pid:012533040 byr:1946

%% hcl:dab227 iyr:2012
%% ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

%% hgt:59cm ecl:zzz
%% eyr:2038 hcl:74454a iyr:2023
%% pid:3556412378 byr:2007

%% Here are some valid passports:

%% pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
%% hcl:#623a2f

%% eyr:2029 ecl:blu cid:129 byr:1989
%% iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

%% hcl:#888785
%% hgt:164cm byr:2001 iyr:2015 cid:88
%% pid:545766238 ecl:hzl
%% eyr:2022

%% iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719

%% Count the number of valid passports - those that have all required
%% fields and valid values. Continue to treat cid as optional. In your
%% batch file, how many passports are valid?

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

is_valid_passport(#passport{birth_year=BirthYear
                           ,issue_year=IssueYear
                           ,expiration_year=ExpirationYear
                           ,height=Height
                           ,hair_color=HairColor
                           ,eye_color=EyeColor
                           ,passport_id=PassportId
                           ,country_id=_CountryId % cid (Country ID) - ignored, missing or not.
                           }) ->
    is_valid_year(BirthYear, 1920, 2002)                  % byr (Birth Year) - four digits; at least 1920 and at most 2002.
        andalso is_valid_year(IssueYear, 2010, 2020)      % iyr (Issue Year) - four digits; at least 2010 and at most 2020.
        andalso is_valid_year(ExpirationYear, 2020, 2030) % eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
        andalso is_valid_height(Height)
        andalso is_valid_hair_color(HairColor)
        andalso is_valid_eye_color(EyeColor)
        andalso is_valid_passport_id(PassportId).

is_valid_year(Year, Min, Max) ->
    Year >= Min andalso Year =< Max.

%% hgt (Height) - a number followed by either cm or in:
is_valid_height('undefined') -> 'false';
is_valid_height(Height) ->
    case re:run(Height, <<"(\\d+)(\\w+)">>, [{'capture', 'all_but_first', 'binary'}]) of
        {'match', [Quantity, Units]} -> is_valid_height(binary_to_integer(Quantity, 10), Units);
        'nomatch' -> 'false'
    end.

%%  If cm, the number must be at least 150 and at most 193.
%%  If in, the number must be at least 59 and at most 76.
is_valid_height(Quantity, <<"cm">>) ->
    Quantity >= 150 andalso Quantity =< 193;
is_valid_height(Quantity, <<"in">>) ->
    Quantity >= 59 andalso Quantity =< 76;
is_valid_height(_Q, _Unit) -> 'false'.

%% hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
is_valid_hair_color(<<"#", Hex:6/binary>>) ->
    lists:all(fun is_hex_char/1, binary_to_list(Hex));
is_valid_hair_color(_HairColor) -> 'false'.

is_hex_char(Digit) when Digit >= $0 andalso Digit =< $9 ->
    'true';
is_hex_char(Char) when Char >= $a andalso Char =< $f ->
    'true';
is_hex_char(_) -> 'false'.

%% ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
is_valid_eye_color('undefined') -> 'false';
is_valid_eye_color(EyeColor) ->
    lists:member(EyeColor, [<<"amb">>, <<"blu">>, <<"brn">>
                           ,<<"gry">>, <<"grn">>, <<"hzl">>
                           ,<<"oth">>
                           ]).

%% pid (Passport ID) - a nine-digit number, including leading zeroes.
is_valid_passport_id(<<PassportId:9/binary>>) ->
    lists:all(fun is_numeric_char/1, binary_to_list(PassportId));
is_valid_passport_id(_PassportId) -> 'false'.

is_numeric_char(Digit) when Digit >= $0 andalso Digit =< $9 ->
    'true';
is_numeric_char(_) -> 'false'.

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
