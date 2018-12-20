FROM centos:7

MAINTAINER ap0sec <apo.t.laboratory@gmail.com>

ADD ./repo/nginx.repo /etc/yum.repos.d/

EXPOSE 80

RUN yum -y update && yum -y install nginx && \
    systemctl enable nginx

ADD ./conf/default.conf /etc/nginx/conf.d/

CMD /sbin/init
