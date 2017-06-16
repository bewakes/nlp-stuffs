from postagger import POSTagger

corpus = [
]

def main():
    f = open('data/mytrain.txt', 'r')
    traindata = []
    for x in f.readlines():
        s = x.strip()
        if s:
            word, tag = s.split()
            traindata.append((word, tag))

    tagger = POSTagger(traindata)
    print(tagger)

    sent = 'good boy is fruit.'
    print(tagger.find_tag_sequence(sent.split()))


if __name__=='__main__':
    main()
