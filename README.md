rec2map
=====

record和json形式的maps之间转换  
Convert between record and json maps

### 使用例子

#### 1、给字段定义类型

```
-record(role, {
    rid                     :: non_neg_integer()
    , srv_id                :: binary()
    , name = <<>>           :: binary()
    , lev = 1               :: pos_integer()
    , partners = []         :: [{record, partner}]
    , m_package             :: {record, m_package}
}).

-record(partner, {
    id                       :: pos_integer()
    , bid                    :: pos_integer()
}).

-record(package, {
    type                     :: pos_integer()
    , items = []             :: [{record, item}]
    , capacity               :: pos_integer()
    , size                   :: non_neg_integer()
}).

-record(m_package, {
    packages                 :: #{pos_integer() => #package{}}
}).

-record(item, {
    id                       :: pos_integer()
    , bid                    :: pos_integer()
}).
```

#### 2、配置gen_rec.sh参数

要生成的头文件路径（`GEN_REC_INC_PATH`）、生成目标文件路径（`GEN_REC_OUT_PATH`）

#### 3、生成rec_term_data.erl文件

```./gen_rec.sh gen```

#### 4、接口使用

* 构造数据

```
Role = #role{
    partners = [#partner{id = 1, bid = 1}]
    , m_package = #m_package{
        packages = #{
            1 => #package{type = 1, items = [#item{id = 1, bid = 1}, #item{id = 2, bid = 2}], capacity = 10, size = 2}
            }
        }
}.
```

* record转maps（rec_term:rec_to_map/1）

```
{ok, Map = #{<<"_rec_name">> => <<"role">>,<<"lev">> => 1,
      <<"m_package">> =>
          #{<<"_rec_name">> => <<"m_package">>,
            <<"packages">> =>
                #{<<"1">> =>
                      #{<<"_rec_name">> => <<"package">>,<<"capacity">> => 10,
                        <<"items">> =>
                            [#{<<"_rec_name">> => <<"item">>,<<"bid">> => 1,<<"id">> => 1},
                             #{<<"_rec_name">> => <<"item">>,<<"bid">> => 2,<<"id">> => 2}],
                        <<"size">> => 2,<<"type">> => 1}}},
      <<"name">> => <<>>,
      <<"partners">> =>
          [#{<<"_rec_name">> => <<"partner">>,<<"bid">> => 1,
             <<"id">> => 1}],
      <<"rid">> => 0,<<"srv_id">> => <<>>}} = rec_term:rec_to_map(Role).
```

* maps转record（rec_term:map_to_rec/2）

```
{ok, #role{rid = 0,srv_id = <<>>,name = <<>>,lev = 1,
          partners = [#partner{id = 1,bid = 1}],
          m_package = #m_package{packages = 
              #{1 =>
                  #package{type = 1,
                           items = [#item{id = 1,bid = 1},#item{id = 2,bid = 2}],
                           capacity = 10,size = 2}}
              }
          }
} = rec_term:map_to_rec(role, Map).
```

### 支持字段类型

| 类型                    | 定义                                                              | 默认值              |
|-----------------------|-----------------------------------------------------------------|------------------|
| 原子                    | atom()                                                          | undefined        |
| 整形                    | integer(), pos_integer(), neg_integer(), non_neg_integer()      | 按类型确定，通常为0       |
| 浮点形                   | float()                                                         | 0.0              |
| Bool                  | boolean()                                                       | false            |
| 二进制                   | binary()                                                        | <<>>             | 
| 记录                    | #record{}, {record,RecName}                                     | 按record子字段生成默认值  |
| 基础类型列表                | [base_type()]                                                   | []               |
| 记录类型列表                | [#record{}], [{record,RecName}]                                 | []               |
| Key为基础类型，Val为基础类型的Map | #{base_type() => base_type()}                                   | #{}              |
| Key为基础类型，Val为记录类型的Map | #{base_type() => #record{}}, #{base_type() => {record,RecName}} | #{}              |
| 基础类型元组                | {base_type(),base_type(),...}                                   | 按tuple子字段类型生成默认值 |
| 基础类型范围类型              | base_type() \| base_type()                                      | 第一个定义值为默认值       |

#### 基础字段类型 base_type()

atom(), integer(), pos_integer(), neg_integer(), non_neg_integer(), float(), boolean(), binary()

### 其他

1、剔除不生成的头文件 在`gen_rec.hrl`的`EXCLUDE_HRLS`宏配置  
2、剔除不生成的记录 在`gen_rec.hrl`的`EXCLUDE_RECORDS`宏配置  

### 支持差异对比

#### 1、对比差异（rec_diff:diff/3、rec_diff:diff/4）
```
2> Role = rec_term_data:rec(role).
#role{rid = undefined,srv_id = undefined,name = <<>>,
      lev = 1,partners = [],m_package = undefined}
3> NewRole = Role#role{partners = [#partner{id = 1, bid = 2}], m_package = #m_package{packages = #{1 => #package{type = 1, items = [#item{id = 1, bid = 2}], capacity = 10, size = 1}}}}.
#role{rid = undefined,srv_id = undefined,name = <<>>,
      lev = 1,
      partners = [#partner{id = 1,bid = 2}],
      m_package = #m_package{packages = #{1 =>
                                              #package{type = 1,
                                                       items = [#item{id = 1,bid = 2}],
                                                       capacity = 10,size = 1}}}}
4> rec_diff:diff(role, Role, NewRole).
{ok,#{<<"m_package">> =>
          #{<<"packages">> =>
                #{<<"1">> =>
                      {rec_diff,add,undefined,
                                #{<<"_rec_name">> => <<"package">>,<<"capacity">> => 10,
                                  <<"items">> =>
                                      [#{<<"_rec_name">> => <<"item">>,<<"bid">> => 2,<<"id">> => 1}],
                                  <<"size">> => 1,<<"type">> => 1}}}},
      <<"partners">> =>
          #{0 =>
                {rec_diff,add,undefined,
                          #{<<"_rec_name">> => <<"partner">>,<<"bid">> => 2,
                            <<"id">> => 1}}}}}
```
* **对比差异支持对比最大层级**，默认最大层级无限制，`rec_diff:diff(role, Role, NewRole, #{max_layer => 2}).` 该例子设置最大层级为2。

#### 2、差异合并（rec_diff:merge/1、rec_diff:merge/2）
```
NewRole2 = NewRole#role{srv_id = <<"test">>, partners = [#partner{id = 2}], m_package = #m_package{packages = #{1 => #package{type = 1, items = [#item{id = 1, bid = 2}, #item{id = 2, bid = 3}], capacity = 1
0, size = 2}}}}.
#role{rid = undefined,srv_id = <<"test">>,name = <<>>,
      lev = 1,
      partners = [#partner{id = 2,bid = undefined}],
      m_package = #m_package{packages = #{1 =>
                                              #package{type = 1,
                                                       items = [#item{id = 1,bid = 2},#item{id = 2,bid = 3}],
                                                       capacity = 10,size = 2}}}}
8> {ok, DiffMap2} = rec_diff:diff(role, NewRole, NewRole2).
{ok,#{<<"m_package">> =>
          #{<<"packages">> =>
                #{<<"1">> =>
                      #{<<"items">> =>
                            #{1 =>
                                  {rec_diff,add,undefined,
                                            #{<<"_rec_name">> => <<"item">>,<<"bid">> => 3,
                                              <<"id">> => 2}}},
                        <<"size">> => {rec_diff,update,1,2}}}},
      <<"partners">> =>
          #{0 =>
                #{<<"bid">> => {rec_diff,update,2,1},
                  <<"id">> => {rec_diff,update,1,2}}},
      <<"srv_id">> => {rec_diff,update,<<>>,<<"test">>}}}
10> NewDiffMap = rec_diff:merge(DiffMap,DiffMap2).
#{<<"m_package">> =>
      #{<<"packages">> =>
            #{<<"1">> =>
                  {rec_diff,add,undefined,
                            #{<<"_rec_name">> => <<"package">>,<<"capacity">> => 10,
                              <<"items">> =>
                                  [#{<<"_rec_name">> => <<"item">>,<<"bid">> => 2,<<"id">> => 1},
                                   #{<<"_rec_name">> => <<"item">>,<<"bid">> => 3,<<"id">> => 2}],
                              <<"size">> => 2,<<"type">> => 1}}}},
  <<"partners">> =>
      #{0 =>
            {rec_diff,add,undefined,
                      #{<<"_rec_name">> => <<"partner">>,<<"bid">> => 1,
                        <<"id">> => 2}}},
  <<"srv_id">> => {rec_diff,update,<<>>,<<"test">>}}
```

#### 3、差异转mongodb更新数据（rec_diff:mongo_update/1）
```
12> rec_diff:mongo_update(NewDiffMap).
#{<<"$set">> =>
      #{<<"m_package.packages.1">> =>
            #{<<"_rec_name">> => <<"package">>,<<"capacity">> => 10,
              <<"items">> =>
                  [#{<<"_rec_name">> => <<"item">>,<<"bid">> => 2,<<"id">> => 1},
                   #{<<"_rec_name">> => <<"item">>,<<"bid">> => 3,<<"id">> => 2}],
              <<"size">> => 2,<<"type">> => 1},
        <<"partners.0">> =>
            #{<<"_rec_name">> => <<"partner">>,<<"bid">> => 1,
              <<"id">> => 2},
        <<"srv_id">> => <<"test">>}}
```