# eorm
orm implementation in Erlang

## Exmaple
```erlang
Ref = eorm_test,
  ok = eorm:run(
    Ref,
    [
      {auto_loading, false},
      {adaptor, emysql},
      {db_args, [
        {user, "root"},
        {password, "root"},
        {host, "127.0.0.1"},
        {database, "eorm_test"},
        {port, 3306},
        {encoding, utf8},
        {pool_size, 8}
      ]}
    ]
  ),
  #eorm_table_meta{} = eorm:load_table(Ref, eorm_demo),
  Obj = eorm_object:new(Ref, eorm_demo),
  Demo = eorm_object:new(Ref, eorm_demo),
  Demo1 = eorm_object:set(guid, 0, Demo),
  Demo2 = eorm_object:set(name, <<"John">>, Demo1),
  Demo3 = eorm_object:flush(Demo2),
  Demo4 = eorm_object:set(age, 20, Demo3),
  Demo5 = eorm_object:flush(Demo4),
  Demo6 = eorm_object:load(Ref, eorm_demo, eorm_object:get(guid, Demo5)),
  Demo6Guid = eorm_object:get(guid, Demo6),
  Demo6Name = eorm_object:get(name, Demo6),
  eorm_object:flush(eorm_object:delete(Demo5)).
  ```
  ## Exmaple Sql
  ```sql
  /*
Navicat MySQL Data Transfer

Source Server         : localhost
Source Server Version : 50704
Source Host           : localhost:3306
Source Database       : eorm_test

Target Server Type    : MYSQL
Target Server Version : 50704
File Encoding         : 65001

Date: 2021-10-20 17:38:49
*/

SET FOREIGN_KEY_CHECKS=0;

-- ----------------------------
-- Table structure for eorm_demo
-- ----------------------------
DROP TABLE IF EXISTS `eorm_demo`;
CREATE TABLE `eorm_demo` (
  `guid` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `name` char(32) NOT NULL DEFAULT 'DefaultName' COMMENT '''test''',
  `age` int(10) unsigned NOT NULL DEFAULT '18',
  PRIMARY KEY (`guid`)
) ENGINE=InnoDB AUTO_INCREMENT=1063 DEFAULT CHARSET=utf8;

```
