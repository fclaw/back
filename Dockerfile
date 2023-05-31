FROM amd64/ubuntu as base

RUN apt update && \
    apt install -y curl && \
    apt install -y tar && \
    apt install -y xz-utils

RUN addgroup --system nixbld && \
    adduser --home /home/nix --disabled-password --gecos "" --shell /bin/bash nix && \
    adduser nix nixbld && \
    mkdir -m 0755 /nix && chown nix /nix && \
    mkdir -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf

CMD /bin/bash -l
USER nix
ENV USER nix
WORKDIR /home/nix

COPY --chown=nix:nix ./nix/install.sh .

RUN touch .bash_profile && /home/nix/install.sh 

ENV PATH="/home/nix/bin:${PATH}"

FROM base as server-build

WORKDIR /build

COPY --chown=nix:nix . .

RUN . /home/nix/.nix-profile/etc/profile.d/nix.sh && \
      nix-shell ./nix/build.nix --verbose --command "stack install --fast -j12 --test"

FROM base as main

WORKDIR /server

COPY --from=server-build --chown=nix:nix /build/bin /server/bin
COPY --from=server-build --chown=nix:nix /build/deploy /server/deploy
COPY --from=server-build --chown=nix:nix /build/migration /server/migration
COPY --from=server-build --chown=nix:nix /build/scripts /server/scripts
COPY --from=server-build --chown=nix:nix /build/nix/deploy.nix /build/shell.nix /build/stack.yaml /build/README.md  /build/Setup.hs /build/LICENSE  /server/

ENTRYPOINT ["/server/deploy/init.sh"]