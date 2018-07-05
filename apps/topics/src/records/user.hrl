-record(user, {
    username,
    email 
}).

to_json(User) when is_record(User, user) ->
    Map = #{
        username => list_to_binary(User#user.username),
        email => list_to_binary(User#user.email)
    },
    jiffy:encode(Map).