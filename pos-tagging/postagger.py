class POSTagger():
    def __init__(self, tagslist, vocab, corpus):
        self._tags = tagslist
        self._vocab = vocab
        self._corpus = corpus

    def find_tag_viterbi(self, input):
        """Implementation of Viterbi Algorithm to find out pos tags of input"""
        pass
