import HashMap "mo:base/HashMap";
import Hash "mo:base/Hash";
import Result "mo:base/Result";
import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import Int "mo:base/Int";
import Blob "mo:base/Blob";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Principal "mo:base/Principal";
import Array "mo:base/Array";
import Iter "mo:base/Iter";
import Buffer "mo:base/Buffer";
import Candy "../candylib";
import ExperimentalCycles "mo:base/ExperimentalCycles";
import Debug "mo:base/Debug";
import AID "mo:aid/AccountId";
import hex "../candylib/hex";
import Time "mo:base/Time";
import Float "mo:base/Float";
import List "mo:base/List";

shared(msg) actor class() = this {

    stable var owner : Principal = msg.caller;
    stable var createdAt : Int = Time.now();

    stable var ownerBucket : Nat = 0; // keeps tracke of how many cycles the owner has been send during license checks
    stable var shareBankStable : [(Principal, Nat)] = []; //keeps track of the number of cycles a particular principal can collect
    stable var minCycles = 4_000_000_000_000; //the minimum cycles the canister needs before it can distribute rewards.
    stable var initializedPrincipalsStable : [(Principal, (Int,Nat))] = [];
    stable var logHistory : List.List<[CandyValue]> = List.nil<[CandyValue]>();
    stable var candyLMCanister : Text = "hdxhu-qqaaa-aaaai-aasnq-cai";

    let license_rate_reset = 2_628_000_000_000_000; //30 days

    //keeps track of who has intialized in the last 30 days
    private var initializedPrincipals : HashMap.HashMap<Principal, (Int,Nat)> = HashMap.fromIter<Principal, (Int,Nat)>(initializedPrincipalsStable.vals(), initializedPrincipalsStable.size(), Principal.equal, Principal.hash);

    //keeps track of the logs
    private var log : Buffer.Buffer<CandyValue> = Buffer.Buffer<CandyValue>(100);


    //handles logs and archiving logs
    private func appendLog(item : CandyValue) : (){

        if(log.size() > 999){
            logHistory := List.append<[CandyValue]>(logHistory, List.make<[CandyValue]>(log.toArray()));
            log := Buffer.Buffer<CandyValue>(1000);
        };
        log.add(item);
    };

    // updates the canister to use for th candy license
    // can only be called by the owner
    public shared(msg) func updateCandyLMCanister(newCanister : Text) : async (){
        assert(msg.caller == owner);

        appendLog(#Class(
            [
                {name = "event"; value=#Text("update_candylmcanister"); immutable= true;},
                {name = "time"; value=#Int(Time.now()); immutable= true;},
                {name = "old"; value=#Text(candyLMCanister); immutable= true;},
                {name = "new"; value=#Text(newCanister); immutable= true;},
            ]
        ));
        candyLMCanister := newCanister;
        //note: a an upgrade is required before this will take effect
    };




    type CycleWalletActor = actor {
        wallet_receive() : async ();
    };

    ////////////
    //Candy Types
    ////////////
    let candy : Candy.Candy = Candy.Candy(
            {min_license_rate = 100_000; //base number of calls
            license_discount_per_check_percent = 10; //10% discount each check
            license_rate_reset = 2_628_000_000_000_000; //30 days
            license_overrun_grace_periods = 100; // you can go 100x over without error
            license_check_cycles = 714_285_714_286; //about a buck
            license_canister = [{canister  = candyLMCanister; percent = 100; distribution_license=null}];
            distribution_license = null; //if you have a distribution license you want your customers to put the principal that owns your nft here
            //the candy license charges $1 upon startup, at least once a month, and per 100,000 or more calls
            }
        );

    type CandyValue = Candy.CandyValue;
    type CandyValueUnstable = Candy.CandyValueUnstable;
    type Workspace = Candy.Workspace;
    type DataZone = Candy.DataZone;
    type DataChunk = Candy.DataChunk;
    type AddressedChunkArray = Candy.AddressedChunkArray;
    type AddressedChunkBuffer = Candy.AddressedChunkBuffer;
    type Property = Candy.Property;
    type PropertyUnstable = Candy.PropertyUnstable;

    //keeps track of the cycles that can be claimed by different principals
    private var shareBank : HashMap.HashMap<Principal, Nat> = HashMap.fromIter<Principal, Nat>(shareBankStable.vals(), shareBankStable.size(), Principal.equal, Principal.hash);


    public shared(msg) func getCurrentLog() : async [CandyValue]{
        assert(msg.caller == owner);
        appendLog(#Class(
                [
                    {name = "event"; value=#Text("log_retrieve"); immutable= true;},
                    {name = "time"; value=#Int(Time.now()); immutable= true;},
                    {name = "caller"; value=#Principal(msg.caller); immutable= true;}
                ]
            ));
        log.toArray();
    };

    public shared(msg) func getLogHistory(page : Nat) : async ([CandyValue], Nat){
        assert(msg.caller == owner);
        let result = switch(List.get<[CandyValue]>(logHistory, page)){
            case(null){([#Empty], List.size<[CandyValue]>(logHistory))};
            case(?v){(v, List.size<[CandyValue]>(logHistory))};
        };

        appendLog(#Class(
            [
                {name = "event"; value=#Text("log_retrieve_history"); immutable= true;},
                {name = "time"; value=#Int(Time.now()); immutable= true;},
                {name = "page"; value=#Nat(page); immutable= true;}
            ]
        ));
        return result;
    };


    // returns the number of cycles a particular principal has on file and can collect
    public shared(msg) func cyclesBalanceOf(p : Text) : async ?Nat {
        assert(msg.caller == Principal.fromText(p) or msg.caller == owner);
        return shareBank.get(Principal.fromText(p));
    };

    // changes the owner of the canister
    // can only be called by the owner
    public shared(msg) func changeOwner(newPrincipal: Text){
        assert(owner == msg.caller);
        appendLog(#Class(
            [
                {name = "event"; value=#Text("change_owner"); immutable= true;},
                {name = "time"; value=#Int(Time.now()); immutable= true;},
                {name = "old"; value=#Principal(owner); immutable= true;},
                {name = "new"; value=#Principal(Principal.fromText(newPrincipal)); immutable= true;},
            ]
        ));
        owner := Principal.fromText(newPrincipal);
    };

    // returns the owner of the canister
    public shared(msg) func getOwner() : async Principal{
        return owner;
    };

    // called by the candy library to send cycles to this canister
    public func __updateLicense(amount : Nat, distribution_license: ?Text) : async Bool{
        assert(ExperimentalCycles.available() > 50_000_000);
        //make sure this principal hasn't initialized in last 30 days
        if(amount == 0){
            switch(initializedPrincipals.get(msg.caller)){
                case(null){};
                case(?val){
                    if(Time.now() < val.0 + license_rate_reset){
                        //gets 10 free initializations a month
                        if(val.1 < 10){
                            initializedPrincipals.put(msg.caller, (val.0, val.1 + 1));
                            appendLog(#Class(
                                [
                                    {name = "event"; value=#Text("free_init"); immutable= true;},
                                    {name = "time"; value=#Int(Time.now()); immutable= true;},
                                    {name = "principal"; value=#Principal(msg.caller); immutable= true;},
                                    {name = "occurance"; value=#Nat(val.1+1); immutable= true;},

                                ]
                            ));
                            //they have inited within the last 30 days. refund
                            return true;
                        };
                    };
                };
            };
        };

        initializedPrincipals.put(msg.caller, (Time.now(),0));

        var shortage = 0;
        let balance = ExperimentalCycles.balance();

        if(balance < minCycles){
            shortage :=  minCycles - ExperimentalCycles.balance();
        };

        var licenseCycles = ExperimentalCycles.accept(ExperimentalCycles.available());

        if(shortage > 0 and shortage > licenseCycles){
            //do nothing. These all need to be reserved
            return true;
        } else if(shortage > 0){
            licenseCycles := licenseCycles - shortage;
        };

        var share = 0;

        ownerBucket += (licenseCycles - share);

        appendLog(#Class(
            [
                {name = "event"; value=#Text("license_update"); immutable= true;},
                {name = "time"; value=#Int(Time.now()); immutable= true;},
                {name = "principal"; value=#Principal(msg.caller); immutable= true;},
                {name = "amount"; value=#Nat(amount); immutable= true;},
                {name = "owner_cycles"; value=#Nat(licenseCycles); immutable= true;},
                {name = "share_cycles"; value=#Nat(share); immutable= true;},

            ]
        ));


        return true;
    };


    //lets the owner withdraw the amount in ownerBucket
    public shared(msg) func wallet_withdraw(cycle_wallet: Text, amount : Nat) : async Bool {
        assert(owner == msg.caller and ownerBucket > amount);
        let remoteWallet : CycleWalletActor = actor(cycle_wallet);
        ExperimentalCycles.add(amount);
        let resultAmount = remoteWallet.wallet_receive();
        appendLog(#Class(
            [
                {name = "event"; value=#Text("wallet_withdraw"); immutable= true;},
                {name = "time"; value=#Int(Time.now()); immutable= true;},
                {name = "principal"; value=#Principal(msg.caller); immutable= true;},
                {name = "amount"; value=#Nat(amount); immutable= true;},
                {name = "cycle_wallet"; value=#Text(cycle_wallet); immutable= true;},
            ]
        ));
        return true;
    };



    public shared({caller}) func wallet_receive() : async () {
        appendLog(#Class(
            [
                {name = "event"; value=#Text("wallet_receive"); immutable= true;},
                {name = "time"; value=#Int(Time.now()); immutable= true;},
                {name = "available"; value=#Nat(ExperimentalCycles.available()); immutable= true;},
                {name = "caller"; value=#Principal(caller); immutable= true;},

            ]
        ));
        ignore ExperimentalCycles.accept(ExperimentalCycles.available());
    };


    // upgrade code
    system func preupgrade() {
        //upgradeBitMapStore := Iter.toArray(bitMapStore.entries());

        appendLog(#Class(
            [
                {name = "event"; value=#Text("pre_upgrade"); immutable= true;},
                {name = "time"; value=#Int(Time.now()); immutable= true;},

            ]
        ));

        shareBankStable := Iter.toArray(shareBank.entries());
        initializedPrincipalsStable := Iter.toArray(initializedPrincipals.entries());
        logHistory := List.append<[CandyValue]>(logHistory, List.make<[CandyValue]>(log.toArray()));

    };

    // upgrade code
    system func postupgrade() {

        appendLog(#Class(
            [
                {name = "event"; value=#Text("post_upgrade"); immutable= true;},
                {name = "time"; value=#Int(Time.now()); immutable= true;},

            ]
        ));


        shareBankStable := [];
        initializedPrincipalsStable := [];


    };
};
