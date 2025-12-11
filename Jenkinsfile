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

    parameters {
        string(name: 'R6RS_SCHEMES', defaultValue: 'capyscheme chezscheme guile ikarus ironscheme larceny loko mosh racket sagittarius ypsilon', description: '')
        string(name: 'R7RS_SCHEMES', defaultValue: 'capyscheme chibi chicken cyclone gambit foment gauche guile kawa larceny loko meevax mit-scheme mosh racket sagittarius skint stklos tr7 ypsilon', description: '')
    }

    stages {
        stage('Build and install') {
            when { not { branch 'release' } }
            steps {
                sh "make build-chibi"
                sh "make install"
            }
        }

        stage('Test') {
            when { not { branch 'release' } }
            parallel {
                stage('R6RS') {
                    steps {
                        script {
                            R7RS_SCHEMES.split().each { SCHEME ->
                                stage("${SCHEME} R6RS") {
                                    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                        sh "make SCHEME=${SCHEME} test-r6rs-docker"
                                    }
                                }
                            }
                        }
                    }
                }

                stage('R7RS') {
                    steps {
                        script {
                            R7RS_SCHEMES.split().each { SCHEME ->
                                stage("${SCHEME} R7RS") {
                                    catchError(buildResult: 'SUCCESS', stageResult: 'FAILURE') {
                                        sh "make SCHEME=${SCHEME} test-r7rs-docker"
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        stage('.deb package') {
            steps {
                sh "make deb"
                archiveArtifacts artifacts: '*.deb', allowEmptyArchive: true, fingerprint: true, onlyIfSuccessful: true
            }
        }
    }
}
