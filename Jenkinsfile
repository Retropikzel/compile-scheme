pipeline {

    agent {
        dockerfile {
            label 'docker-x86_64'
            filename 'Dockerfile.jenkins'
            args '--user=root --privileged -v /var/run/docker.sock:/var/run/docker.sock'
        }
    }

    options {
        disableConcurrentBuilds()
        buildDiscarder(logRotator(numToKeepStr: '10', artifactNumToKeepStr: '10'))
    }

    stages {
        stage('Build and install') {
            steps {
                sh "make build-chibi"
                sh "make install"
            }
        }

        stage('Test R6RS') {
            steps {
                script {
                    def SCHEMES = "chezscheme guile ikarus ironscheme larceny loko mosh racket sagittarius ypsilon"
                    SCHEMES.split().each { SCHEME ->
                        stage("${SCHEME} R6RS") {
                            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                sh "make SCHEME=${SCHEME} test-r6rs-docker"
                            }
                        }
                    }
                }
            }
        }

        stage('Test R7RS') {
            steps {
                script {
                    def SCHEMES = "chibi chicken cyclone gambit foment gauche guile kawa larceny loko meevax mit-scheme mosh racket sagittarius skint stklos tr7 ypsilon"
                    SCHEMES.split().each { SCHEME ->
                        stage("${SCHEME} R6RS") {
                            catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                sh "make SCHEME=${SCHEME} test-r6rs-docker"
                            }
                        }
                    }
                }
            }
        }
    }
}
