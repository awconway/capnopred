FROM rocker/tidyverse

RUN install2.r --error \
    targets \
    tarchetypes \
    languageserver \
    httpgd \
    tidymodels \
    glmnet \
    TraMineR \
    xgboost \
    randomForest \
    vip \
    zoo \
    flextable \
    officedown \
    officer \
    gtsummary

    
RUN installGithub.r \
    ML4LHS/runway



# [Option] Install zsh
ARG INSTALL_ZSH="true"
# [Option] Upgrade OS packages to their latest versions
ARG UPGRADE_PACKAGES="false"

# Install needed packages and setup non-root user. Use a separate RUN statement to add your own dependencies.
ARG USERNAME=docker
ARG USER_UID=2000
ARG USER_GID=$USER_UID
COPY library-scripts/*.sh /tmp/library-scripts/
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && /bin/bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" \
    && apt-get -y install libzip-dev \
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* /tmp/library-scripts

RUN apt-get update && apt-get install -y libglpk40 libxt6 fonts-firacode python3-pip

RUN python3 -m pip install radian
