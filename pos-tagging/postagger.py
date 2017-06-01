from itertools import product

class POSTagger():
    def __init__(self, tagslist, vocab, corpus):
        self._tags = ['*'] + tagslist
        self._vocab = vocab
        self._corpus = corpus
        self._tag_bigrams = list(product(self._tags, self._tags))

    def find_tag_viterbi(self, input):
        """Implementation of Viterbi Algorithm to find out pos tags of input"""
        # store the pi values for i=0,1..n
        # n is length of input
        pi_list = [1 if x==('*','*') else 0 for x in self._tag_bigrams] # initialization

        for i, word in enumerate(input):
            new_pi = []
            for tag_bigram in self._tag_bigrams:
                pass
        pass
