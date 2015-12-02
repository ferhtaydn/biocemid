#!/bin/bash

word2vec_distances() {
	while read p; do
		$1 $2 $p
	done < $3
}

# example: word2vec_distances ./distance_files ~/Desktop/bc3_word2vec_results/phrase1_eval/bc3_phrase1_vectors.bin files/word2vec_distance_files_run_params.txt

# while read p; do ../word2vec_extension/distance_files ../oa/oa_phrases1_vectors.bin $p; done < ../word2vec_distance_files_run_params_2.txt