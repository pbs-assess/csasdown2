all: paper.pdf

paper.pdf: paper.md refs.bib
	docker run --rm --volume "$(PWD)":/data --user $(shell id -u):$(shell id -g) \
		openjournals/inara -o pdf,crossref paper.md
