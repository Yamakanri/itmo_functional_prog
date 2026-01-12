-module(proc_parser_test).
-include_lib("eunit/include/eunit.hrl").

convert_strings_to_atoms_test() ->
    ?_test(
        fun() ->
            ?assertEqual([linear, lagrange], main:convert_strings_to_atoms(["linear","lagrange"])),
            ?assertEqual([], main:convert_strings_to_atoms([]))
        end
    ).
