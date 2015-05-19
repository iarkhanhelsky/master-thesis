
require(png)

source('r/statistics.r')

lenna <- readPNG('res/Lenna.png')

lenna.blue.bits <- (lenna[, , 3] * 255) %% 2

lipsum <-"Williamsburg selfies food truck bespoke. Roof party seitan meditation
squid. Lumbersexual cred forage, paleo pop-up biodiesel flexitarian selvage
disrupt Pitchfork Pinterest quinoa. Ugh swag selfies kitsch, forage try-hard
meditation chillwave ethical flexitarian craft beer Odd Future art party
tousled. Health goth swag actually, XOXO small batch pork belly hella ennui
selfies Neutra PBR gentrify. You probably haven't heard of them meggings
wayfarers, literally blog art party four loko sriracha cornhole Intelligentsia
tattooed fixie gastropub pork belly. Retro scenester Thundercats banjo."

text.bits <- c( # Converting to vector
                # We need transpose matrix for proper vector expansion
                 outer( # Map char list to bits matrix
                       sapply(lipsum, utf8ToInt), # Each char to byte
                       0:7,
                       function(char, bit) bitwShiftR(char, bit) %% 2))

lenna.bits.length <- length(lenna.blue.bits)
text.bits.length <- length(text.bits)
print(mean(text.bits))
print(mean(lenna.blue.bits))
bits.mix <- (lenna.blue.bits + # Take all image bits
    c(rep(0, lenna.bits.length - text.bits.length), text.bits) # Add shifted
    # text bits
    ) %% 2 # take sum by module of 2

# Now we gonna test our hypotesis

print(criterium.periodogram(2*bits.mix - 1))



