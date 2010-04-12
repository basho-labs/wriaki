-module(rec_obj).

-export([create/3,
         create_siblings/4,
         get_vclock/1,
         set_vclock/2,
         bucket/1,
         key/1,
         get_value/1,
         set_value/2,
         get_links/1,
         set_links/2,
         remove_links/3,
         add_link/2,
         get_content_type/1,
         set_content_type/2,
         get_siblings/1,
         has_siblings/1,
         get_json_field/2,
         set_json_field/3]).
         
-record(rec_obj, {bucket,
                  key,
                  value,
                  links=[],
                  ctype="application/json",
                  vclock=""}).
-record(rec_sib, {bucket,
                  key,
                  vclock,
                  sibs}).

create(Bucket, Key, Value) when is_binary(Bucket), is_binary(Key) ->
    #rec_obj{bucket=Bucket, key=Key, value=Value}.

create_siblings(Bucket, Key, Vclock, Siblings)
  when is_binary(Bucket), is_binary(Key),
       is_list(Vclock), is_list(Siblings) ->
    #rec_sib{bucket=Bucket, key=Key, vclock=Vclock, sibs=Siblings}.

get_vclock(#rec_obj{vclock=Vclock}) ->
    Vclock;
get_vclock(#rec_sib{vclock=Vclock}) ->
    Vclock.

set_vclock(Obj, Vclock) ->
    Obj#rec_obj{vclock=Vclock}.

bucket(#rec_obj{bucket=Bucket}) ->
    Bucket;
bucket(#rec_sib{bucket=Bucket}) ->
    Bucket.

key(#rec_obj{key=Key}) ->
    Key;
key(#rec_sib{key=Key}) ->
    Key.

get_value(#rec_obj{value=Value}) ->
    Value.

set_value(Obj, Value) ->
    Obj#rec_obj{value=Value}.

get_links(#rec_obj{links=Links}) ->
    Links.

remove_links(Obj, Bucket, Tag) ->
    set_links(Obj, lists:filter(link_filter(Bucket, Tag), get_links(Obj))).

link_filter('_', '_') ->
    fun(_) -> true end;
link_filter(Bucket, '_') ->
    fun({{B,_},_}) -> B =:= Bucket end;
link_filter('_', Tag) ->
    fun({_,T}) -> T =:= Tag end;
link_filter(Bucket, Tag) ->
    fun({{B,_},T}) -> B =:= Bucket andalso T =:= Tag end.

set_links(Obj, Links) when is_list(Links) ->
    Obj#rec_obj{links=Links}.

add_link(Obj, Link={{_,_},_}) ->
    Links = get_links(Obj),
    case lists:member(Link, Links) of
        true -> Obj;
        false -> set_links(Obj, [Link|Links])
    end.

get_content_type(#rec_obj{ctype=ContentType}) ->
    ContentType.

set_content_type(Obj, ContentType) ->
    Obj#rec_obj{ctype=ContentType}.

get_siblings(#rec_sib{bucket=Bucket, key=Key, vclock=Vclock, sibs=Siblings}) ->
    [set_links(
       set_content_type(
         set_vclock(
           create(Bucket, Key, Value),
           Vclock),
         ContentType),
       Links)
     || {ContentType, Links, Value} <- Siblings].

has_siblings(#rec_sib{}) -> true;
has_siblings(#rec_obj{}) -> false.


get_json_field(RecObj, Field) ->
    {struct, Props} = rec_obj:get_value(RecObj),
    proplists:get_value(Field, Props).

set_json_field(RecObj, Field, Value) ->
    {struct, Props} = rec_obj:get_value(RecObj),
    NewProps = [{Field, Value}
                | [ {F, V} || {F, V} <- Props, F =/= Field ]],
    rec_obj:set_value(RecObj, {struct, NewProps}).
    
