FROM haskell:9.12.2-slim-bookworm AS builder
RUN echo cabal update date: 2026-01-05
RUN cabal update --verbose=2

WORKDIR /hs-fs-filter
COPY --link ./hs-fs-filter.cabal ./
RUN cabal update --verbose=2
RUN cabal build --only-dependencies
COPY --link ./app/ ./app/
COPY --link ./src/ ./src/
RUN cabal build
RUN cp $( cabal list-bin hs-fs-filter | fgrep --max-count=1 hs-fs-filter ) /usr/local/bin/
RUN which hs-fs-filter
RUN mkdir -p demo && touch demo/file.txt
RUN echo "--- Demo: finding files ---" && ls demo | hs-fs-filter --only-files
RUN echo "--- Demo: finding dirs ---" && ls | hs-fs-filter --only-dirs

FROM debian:bookworm-slim
COPY --link --from=builder /usr/local/bin/hs-fs-filter /usr/local/bin/

CMD ["/usr/local/bin/hs-fs-filter"]
