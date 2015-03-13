# --- !Ups

create table serviceLog (
  userId    bigint not null,
  src       varchar(3) not null,
  dst       varchar(3) not null,
  txt       text not null,
  service   varchar(255) not null,
  used      boolean not null,
  time      timestamp default current_timestamp not null,
  constraint sl_userId foreign key (userId) references userAccount(id)
);

# --- !Downs

drop table if exists serviceLog;