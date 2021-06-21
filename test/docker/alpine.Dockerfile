# syntax=docker/dockerfile:1
FROM alpine
COPY . /code
RUN apk add gcc libc-dev git make ncurses
WORKDIR code
