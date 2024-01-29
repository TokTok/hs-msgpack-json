FROM toxchat/toktok-stack:haskell AS build

RUN ["rm", "-rf", "/work/hs-msgpack-json"]
COPY --chown=builder:users . /work/hs-msgpack-json
RUN ["stack", "install", "msgpack-json"]

FROM scratch
COPY --from=build /home/builder/.local/ /
