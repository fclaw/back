FROM amd64/ubuntu as base

RUN apt update && \
    apt install -y curl && \
    apt install -y tar && \
    apt install -y xz-utils && \
    apt install -y locales

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen

ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8  

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
      nix-shell ./nix/build.nix \
     --log-format bar-with-logs \ 
     --verbose --command \ 
     "./scripts/api-generator.sh sendgrid-openapiv3.yaml SendGrid sendgrid . && \
     ./scripts/api-generator.sh github-openapiv3.yaml GitHub github . && \
      stack install --system-ghc --fast -j12 --test"

FROM base as main

ARG sendgrid_key
ARG telegram_bot_key
ARG github_tth_docs_key
ARG github_tth_main_key
ARG github_tth_css
ARG mute_500
ARG env_yaml
ARG env_captcha_key

ENV MUTE_500 ${mute_500}
ENV YAML_ENV ${env_yaml} 


EXPOSE 12000/tcp

WORKDIR /server

RUN echo \
"sendgrid: $sendgrid_key\n"\
"telegrambot: $telegram_bot_key\n"\
"github:\n"\
"   frontDocs:\n"\
"      repo: turkish-trade-house-docs\n"\ 
"      key: $github_tth_docs_key\n"\
"      resources: [home.txt, about.txt, services.txt]\n"\
"   front:\n"\
"      repo: turkish-trade-house\n"\ 
"      key: $github_tth_main_key\n"\
"      resources: []\n"\
"   frontCSS:\n"\
"      repo: turkish-trade-house-style-and-css\n"\
"      key: $github_tth_css\n"\
"      resources: []\n"\
"captchakey: $env_captcha_key"\
> /server/env.yaml

COPY --from=server-build --chown=nix:nix /build/bin /server/bin
COPY --from=server-build --chown=nix:nix /build/deploy /server/deploy
COPY --from=server-build --chown=nix:nix /build/migration /server/migration
COPY --from=server-build --chown=nix:nix /build/tls /server/tls
COPY --from=server-build --chown=nix:nix /build/package.yaml /build/nix/deploy.nix /build/stack.yaml /build/Setup.hs /server/

ENTRYPOINT ["/server/deploy/init.sh"]