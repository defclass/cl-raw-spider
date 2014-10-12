CREATE TABLE `dg-good` (
  `good-id` bigint not null AUTO_INCREMENT  COMMENT 'goods ID',
  `source` varchar(128) NOT NULL DEFAULT '' COMMENT '来源的导购网站',
  `min-image` varchar(128) NOT NULL DEFAULT '' COMMENT '列表页小图地址',
  `mall-url` varchar(128) NOT NULL DEFAULT '' COMMENT '购买地址',
  `headline` varchar(128) NOT NULL DEFAULT '' COMMENT '标题',
  `create-time` int(10) unsigned NOT NULL DEFAULT '0' COMMENT '创建日期',
  `belong-to` varchar(64) NOT NULL DEFAULT '' COMMENT '所属商城',
  `content` text COMMENT '内容页',
  `category` int(10) NOT NULL DEFAULT 0 COMMENT '分类ID',
  `participle` varchar(64) NOT NULL DEFAULT '' COMMENT '分词',
  `click-total` int(10) NOT NULL DEFAULT 0 COMMENT '点击数',
  PRIMARY KEY (`good-id`)
)ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='导购表'
