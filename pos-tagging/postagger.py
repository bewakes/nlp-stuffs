from itertools import product

######################################################################
##
## THEORETICAL REFERENCE FROM:
## http://www.cs.columbia.edu/~mcollins/courses/nlp2011/notes/hmms.pdf
##
######################################################################

STOP = '.'
PAUSE = ','

class POSTagger():
    def __init__(self, corpus, vocab=[]):
        """
        Initialize the POS tagger
        - taglist: list of all the tags used, including 'STOP' and beginning '*'
        - vocab: this is probably generated from corpus
        - corpus: list of (word, tag) pairs (hopefully)
        """
        self._vocab = vocab
        self._corpus = corpus

        self._tag_counts = {}
        self._word_tag_counts = {}
        self._tag_trigram_counts = {}
        self._tag_bigram_counts = {}
        # create a probability distribution
        self._update_counts(corpus)

        # update other params
        self._tags = ['*'] + list(self._tag_counts.keys())
        self._tag_bigrams = list(product(self._tags, self._tags))
        self._vocab = list(self._word_tag_counts.keys())

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
                        prob = pi_list[i]* self._e(word, v) * self._q(v, w, u)
                        if prob>mx:
                            mx=prob
                new_pi.append(mx)
            pi_list.append(new_pi)
        return max(map(lambda x: x* self._q(STOP, u, v), pi_list[-1]))

    def find_tag_sequence(self, input):
        """
        Similar to above viterbi implementation except that this keeps track
         of tags also, unlike above which just calculates max,
        Plus, this is going to be used not the above

        input:  is a list
        """
        pi_list = [[1 if x==('*','*') else 0 for x in self._tag_bigrams]] # initialization

        tag_seq = [0 for _ in input] + [0] # one extra because of initial *, which will later be discarded

        for i, word in enumerate(input):
            new_pi = [] # stores new values for pi, and is appended to pi_list
            for k, tag_bigram in enumerate(self._tag_bigrams):
                u,v = tag_bigram
                mx = 0
                for j, tag_bigrm in enumerate(self._tag_bigrams):
                    w, U = tag_bigrm
                    if u == U:
                        prob = pi_list[i][j]* self._e(word, v) * self._q(v, w, u)
                        if prob>mx:
                            tag_seq[i] = j
                            mx=prob
                new_pi.append(mx)
            pi_list.append(new_pi)

        last_pi_list = list(map(lambda x: x* self._q(STOP, u, v), pi_list[-1]))

        mx = max(last_pi_list)
        tag_seq[-1] = last_pi_list.index(mx)

        return list(map(lambda x: self._tag_bigrams[1], tag_seq))


    def _q(self, tag, tag1, tag2):
        """
        naive implementation, later can be made efficient using other DS
        returns probability of occuring 'tag' given tag1 and tag2 occur sequentially
        """
        return self._count3(tag1, tag2, tag)/self._count2(tag1,tag2)

    def _e(self, word, tag):
        """
        returns probability of occurence of 'word' given 'tag'
        """
        # _count(tag, word) is the number of occurences of tag and word together
        return self._count(tag, word) / self._count0(tag)

    # TODO: implement _count0, _count, _count2 and count3
    def _count0(self, tag):
        return self._tag_counts.get(tag, 1) # LETS NOT keep this zero, as it is denominator

    def _count(self, tag, word):
        return self._word_tag_counts.get(word, {}).get(tag, 0)

    def _count2(self, tag1, tag2):
        return self._tag_bigram_counts.get(tag1, {}).get(tag2,1) # LETS NOT keep this zero, as it is denominator

    def _count3(self, tag1, tag2, tag):
        return self._tag_trigram_counts.get(tag1, {}).get(tag2, {}).get(tag,0)


    def _update_counts(self, vocab):
        """
        Create counts from vocab
        """
        prevtag2 = "*"
        prevtag1 = "*"
        for (word, tag) in vocab:
            # increment tag count
            tagcnt = self._tag_counts.get(tag, 0)
            self._tag_counts[tag] = tagcnt+1
            # increment word_tag_count
            word_tag_dict = self._word_tag_counts.get(word, {})
            tagcount = word_tag_dict.get(tag, 0)
            word_tag_dict[tag] = tagcount+1
            self._word_tag_counts[word] = dict(word_tag_dict)
            # update tag bigram counts
            prev1tag_bigrm = self._tag_bigram_counts.get(prevtag1, {})
            curr_tag_count = prev1tag_bigrm.get(tag, 0)
            prev1tag_bigrm[tag] = curr_tag_count+1
            self._tag_bigram_counts[prevtag1] = dict(prev1tag_bigrm)
            # update tag trigram counts
            prev2tag_trigrm = self._tag_trigram_counts.get(prevtag2, {})
            prev1tag_bigrm = prev2tag_trigrm.get(prevtag1, {})
            curr_tag_count = prev1tag_bigrm.get(tag, 0)
            prev1tag_bigrm[tag] = curr_tag_count + 1
            prev2tag_trigrm[prevtag1] = dict(prev1tag_bigrm)
            self._tag_trigram_counts[prevtag2] = dict(prev2tag_trigrm)
            # update prevtag2 and prevtag1
            if tag == '.': # . for stop
                ## reset prevtag1 and prevtag2
                prevtag1 = prevtag2 = "*"
            else:
                prevtag2 = prevtag1
                prevtag1 = tag
        ## DONE with counts creation

    def __str__(self):
        print(self._vocab)
        return 'tag counts: {}\ntag bigram counts: {}\ntag trigram counts: {}\nword tag counts: {}'.\
                format(self._tag_counts, self._tag_bigram_counts, self._tag_trigram_counts, self._word_tag_counts)
