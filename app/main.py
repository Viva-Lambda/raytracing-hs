if __name__ == "__main__":
    im = 256
    ih = 256
    print("P3\n",im," ",ih,"\n255\n")
    for j in range(ih, 0, -1):
        for i in range(im):
            r = i / (im-1)
            g = j / (ih -1)
            b = 0.25

            ir = int(255.999 * r)
            ig = int(255.999 * g)
            ib = int(255.999 * b)

            print(ir, " ", ig, " ", ib)
