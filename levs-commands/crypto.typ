#import "./math.typ": *

//////////////// Cryptography Commands for Typst ////////////////
/**
 * Keep in mind that these are just my (Lev's) personal commands and preferences.
 * Feel free to use them, but remember that there are many other ways to do things.
 */

#let pk = $mono("pk")$
#let mpk = $mono("mpk")$
#let sk = $mono("sk")$
#let vk = $mono("vk")$
#let ek = $mono("ek")$
#let Encr = $mono("Encr")$
#let skEncr = $sk\.mono("Encr")$
#let skDecr = $sk\.mono("Decr")$

#let advers = $calA$
#let advers2 = $calB$
#let Sim = $sans("Sim")$
#let Real = $sans("Real")$
#let View = $sans("View")$
#let Ideal = $sans("Ideal")$
#let Exp = $sans("Exp")$
#let Adv = $sans("Adv")$
#let Hyb = $sans("Hyb")$
#let compInd = $approx^c$
#let Game = $sans("Game")$
#let Setup = $sans("Setup")$
#let negl = $sans("negl")$
#let poly = $sans("poly")$
#let ct = $mono("ct")$
#let ciphertext = $ct$

#let Oracle = $calO$
#let Queries = $calQ$

///////////////// Information Theory in Crypto /////////////////
#let Hmin = $H_oo$

///////////////// (Zero Knowledge) Proofs /////////////////


/////////////// Generic Protocol Commands for Commitments ////////////////
#let GSetup = $bold("GlobalSetup")$
#let Setup = $bold("Setup")$
#let Gen = $bold("Gen")$
#let SetupNB = $"Setup"$
#let Eval = $bold("Eval")$
#let Init = $bold("Init")$
#let Commit = $"Commit"$
#let Open = $bold("Open")$
#let Verify = $bold("Verify")$
#let KeyGen = $bold("KeyGen")$
#let Sign = $"Sign"$
#let Agg = $"Agg"$
#let AggVerify = $"AggVerify"$
#let Prove = $bold("Prove")$
#let Valid = $"Valid"$
#let comm = $mono("comm")$
#let ck = $mono("ck")$
#let aux = $mono("aux")$

///////////////// KEMs /////////////////
#let Encap = $sans("Encap")$
#let Decap = $sans("Decap")$

//////////// Extractability and Witnesses ////////////
#let Ext = $sans("Ext")$
#let randSamp = $<-^"$"$

///////////// One-Time Program /////////////
#let OTP = $sans("OTP")$
#let VOTP = $"Ver-"sans("OTP")$
#let otp = $sans("otp")$
#let votp = $sans("v-otp")$
#let crs = $mono("crs")$

////////// Garbled Circuits //////////
#let Garble = $sans("Garble")$

////////////// UC ////////////
#let sid = $"sid"$
#let ssid = $"ssid"$

