-module(avl_tree_proper_tests).
-include_lib("proper/include/proper.hrl").

-export([prop_associativity_test/0, prop_neutral_element_test/0, 
         prop_insert_find_test/0]).

-define(KEY_FILL_VALUE, test_value).

prop_associativity_test() ->
    ?FORALL(
        {Keys1, Keys2, Keys3},
        {list(int()), list(int()), list(int())},
        begin
            Dict1 = store_keys(Keys1),
            Dict2 = store_keys(Keys2),
            Dict3 = store_keys(Keys3),

            Union1 = tree_cur:concat(
                tree_cur:concat(Dict1, Dict2), Dict3
            ),

            Union2 = tree_cur:concat(
                tree_cur:concat(Dict1, Dict3), Dict2
            ),

            tree_cur:equal(Union1, Union2)
        end
    ).

prop_neutral_element_test() ->
    ?FORALL(
        {Keys},
        {list(int())},
        begin
            Neutral = tree_cur:new(),
            Dict = store_keys(Keys),

            Union1 = tree_cur:concat(Neutral, Dict),
            Union2 = tree_cur:concat(Dict, Neutral),

            tree_cur:equal(Union1, Union2) andalso tree_cur:equal(Union1, Dict)
        end
    ).

prop_insert_find_test() ->
    ?FORALL(
        {Keys, Key, Value},
        {list(int()), int(), int()},
        begin
            Dict = store_keys(Keys),
            NewDict = tree_cur:insert(Dict, Key, Value),
            tree_cur:find(NewDict, Key) =:= {ok, Value}
        end
    ).


%Саппорт функция для создания тестовых деревьев из списка ключей
store_keys(Keys) -> store_keys(Keys, tree_cur:new()).

store_keys([], Dict) ->
    Dict;
store_keys([H | T], Dict) ->
    NewDict = tree_cur:insert(Dict, H, ?KEY_FILL_VALUE),
    store_keys(T, NewDict).
