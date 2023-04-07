REGISTRY_HOST=docker.io
USERNAME=degauss
NAME=$(shell basename "$(CURDIR)")
IMAGE=$(REGISTRY_HOST)/$(USERNAME)/$(NAME)

.PHONY: build test shell release clean

build:
	docker build -t $(IMAGE) .

test:
	docker run --rm -v "${PWD}/test":/tmp $(IMAGE) sim_test_data_new_prepped_geocoder_3.3.0_score_threshold_0.5_census_block_group_0.6.0_2010.csv

shell:
	docker run --rm -it --entrypoint=/bin/bash -v "${PWD}/test":/tmp $(IMAGE)

release:
ifndef VERSION
	$(error VERSION is not set. Usage: "make release VERSION=X.X")
endif
ifndef DOCKER_USERNAME
	$(error DOCKER_USERNAME is not set)
endif
ifndef DOCKER_PAT
	$(error DOCKER_PAT is not set)
endif
	git commit -am "Release for image version $(VERSION)" --allow-empty
	git tag -fa $(VERSION) -m "${VERSION}"
	git push origin ${VERSION}
	git push
	echo "${DOCKER_PAT}" | docker login -u "${DOCKER_USERNAME}" --password-stdin
	docker tag ${IMAGE}:latest ${IMAGE}:${VERSION}
	docker push ${IMAGE}:${VERSION}
	docker push ${IMAGE}:latest

clean:
	docker system prune -f
