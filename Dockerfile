FROM fpco/alpine-haskell-stack:8.6.5 AS build

RUN mkdir /opt/build
COPY . /opt/build
RUN apk update && \
    apk add upx
RUN cd /opt/build && \
    stack build --system-ghc --copy-bins --local-bin-path /opt/build && \
    upx -o bot radiofx-tg-bot

FROM alpine:3.9

RUN apk update && \
    apk add libffi gmp && \
    rm -rf /var/cache/apk/*
RUN mkdir /opt/app
WORKDIR /opt/app
COPY --from=build /opt/build/bot /opt/app/bot
CMD ["/opt/app/bot"]
