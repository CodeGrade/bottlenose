FROM orca-grader-base:latest

RUN ["apt", "update"]
RUN ["apt", "install", "openjdk-11-jdk-headless", "-y"]

USER orca-grader