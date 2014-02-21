# --- !Ups

create table userAccount (
  id             bigint not null auto_increment,
  username       varchar(255) not null,
  passHash       varchar(255) not null,
  email          varchar(255) not null,
  authKey        varchar(255) not null,
  primary key(id)
);

create table serviceList (
  userId         bigint not null auto_increment,
  priority       int not null,
  name           varchar(255) not null,
  constraint fk_userId foreign key (userId) references userAccount(id)
);

# --- !Downs

drop table if exists userAccount;
drop table is exists serviceList;