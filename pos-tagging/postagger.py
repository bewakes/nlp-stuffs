from itertools import product

######################################################################
##
## THEORETICAL REFERENCE FROM:
## http://www.cs.columbia.edu/~mcollins/courses/nlp2011/notes/hmms.pdf
##
######################################################################

class POSTagger():
    def __init__(self, tagslist, vocab, corpus):
        self._tags = ['*'] + tagslist
        self._vocab = vocab
        self._corpus = corpus
        self._tag_bigrams = list(product(self._tags, self._tags))

    def _find_tag_viterbi(self, input):
        """Implementation of Viterbi Algorithm to find out pos tags of input"""
        # store the pi values for i=0,1..n
        # n is length of input
        pi_list = [1 if x==('*','*') else 0 for x in self._tag_bigrams] # initialization

        for i, word in enumerate(input):
            new_pi = [] # stores new values for pi, and is appended to pi_list
            for tag_bigram in self._tag_bigrams:
                u,v = tag_bigram
                mx = 0
                for j, tag_bigrm in enumerate(self._tag_bigrams):
                    w, U = tag_bigrm
                    if u == U:
                        prob = pi_list[i]* _e(word, v) * _q(v, w, u)
                        if prob>mx:
                            mx=prob
                new_pi.append(mx)
            pi_list.append(new_pi)
        return max(map(lambda x: x* _q(STOP, u v)))

    def _q(tag1, tag2, tag3):
        pass

    def _e(word, tag):
        pass
