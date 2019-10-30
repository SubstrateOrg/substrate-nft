
#![cfg_attr(not(feature = "std"), no_std)]

/// A runtime module template with necessary imports

/// Feel free to remove or edit this file as needed.
/// If you change the name of this file, make sure to update its references in runtime/src/lib.rs
/// If you remove this file, you can remove those references


/// For more guidance on Substrate modules, see the example module
/// https://github.com/paritytech/substrate/blob/master/srml/example/src/lib.rs

use rstd::prelude::*;
use support::{decl_module, decl_storage, decl_event, Parameter, StorageMap, StorageValue,
	dispatch::Result, ensure, traits::Currency
};
use sr_primitives::{
	traits::{SimpleArithmetic, Bounded, Member, Zero, CheckedSub, CheckedAdd},
	weights::SimpleDispatchInfo
};
use system::ensure_signed;

use rstd::vec::Vec;

pub mod linked_item;
use linked_item::{LinkedItem, LinkedList};

pub mod nft_currency;
use nft_currency::{NFTCurrency};


/// The module's configuration trait.
pub trait Trait: system::Trait {
	// TODO: Add other types and constants required configure this module.

	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
	
	//type TokenId: Parameter + Member + SimpleArithmetic + Bounded + Default + Copy;
	type TokenId: Parameter + Member + SimpleArithmetic + Bounded + Default + Copy + Into<u64>;

	type Currency: Currency<Self::AccountId>;
}

//type BalanceOf<T> = <<T as Trait>::Currency as Currency<<T as system::Trait>::AccountId>>::Balance;

type TokenLinkedItem<T> = LinkedItem<<T as Trait>::TokenId>;
type OwnerToTokenList<T> = LinkedList<OwnerToToken<T>, <T as system::Trait>::AccountId, <T as Trait>::TokenId>;

// This module's storage items.
decl_storage! {
	trait Store for Module<T: Trait> as Nft {

		//#region ERC-721 metadata extension

		pub Symbol get(symbol) config(): Vec<u8>;

		pub Name get(name) config(): Vec<u8>;

		pub TokenURI get(token_uri): map T::TokenId => Vec<u8>;

		//#endregion


		//#region ERC-721 compliant contract

		/// TokenId => TokenOwner
		pub TokenToOwner get(owner_of): map T::TokenId => T::AccountId;

		/// TokenOwner => TokenCount
		pub OwnerCount get(balance_of): map T::AccountId => T::TokenId;

		/// TokenId =>  Account for Approval
		pub TokenToApproval get(get_approved): map T::TokenId => Option<T::AccountId>;

		/// (OwnerAccountId, ApprovalAccountId) =>  isApproval
		pub OwnerToOperator get(is_approved_for_all): map (T::AccountId, T::AccountId) => bool;

		//#endregion


		//#region ERC-721 enumeration extension

		/// total supply of the token
		pub TotalSupply get(total_supply): T::TokenId;

		/// TokenId is token index
		//pub TokenByIndex get(token_by_index): T::TokenId => T::TokenId;

		/// TokenId is token index
		//pub TokenOfOwnerByIndex get(token_of_owner_by_index): map (T::AccountId, T::TokenId) => T::TokenId;

		//#endregion


		//#region Other Index

		/// Owner token linked list, for fast enumeration and transfer
		pub OwnerToToken get(owner_to_token): map (T::AccountId, Option<T::TokenId>) => Option<TokenLinkedItem<T>>;

		//endregion
	}
}

// The module's dispatchable functions.
decl_module! {
	/// The module declaration.
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		// Initializing events
		// this is needed only if you are using events in your module
		//fn deposit_event<T>() = default;
		fn deposit_event() = default;

		/// approve another account to manage a token of your account
		#[weight = SimpleDispatchInfo::FixedNormal(1_000_000)]
		fn approve(origin, to:  Option<T::AccountId>, token_id: T::TokenId) {
			let sender = ensure_signed(origin)?;

			<Self as NFTCurrency<_>>::approve(&sender, &to, token_id)?;

			Self::deposit_event(RawEvent::Approval(sender, to, token_id));
		}

		/// approve another account to manage all tokens of your account
		#[weight = SimpleDispatchInfo::FixedNormal(1_000_000)]
		fn set_approval_for_all(origin, to: T::AccountId, approved: bool) {
			let sender = ensure_signed(origin)?;

			<Self as NFTCurrency<_>>::set_approval_for_all(&sender, &to, approved)?;

			Self::deposit_event(RawEvent::ApprovalForAll(sender, to, approved));
		}

		/// transfer token
		#[weight = SimpleDispatchInfo::FixedNormal(1_000_000)]
		fn transfer_from(origin, from: T::AccountId, to: T::AccountId, token_id: T::TokenId) {
			let sender = ensure_signed(origin)?;

			<Self as NFTCurrency<_>>::transfer_from(&sender, &from, &to, token_id)?;

			Self::deposit_event(RawEvent::Transfer(from, to, token_id));
		}

		// safe transfer token
		#[weight = SimpleDispatchInfo::FixedNormal(1_000_000)]
		fn safe_transfer_from(origin, from: T::AccountId, to: T::AccountId, token_id: T::TokenId) {

			let sender = ensure_signed(origin)?;

			<Self as NFTCurrency<_>>::safe_transfer_from(&sender, &from, &to, token_id)?;
			
			Self::deposit_event(RawEvent::Transfer(from, to, token_id));
		}

		/// create token for your self, maybe for test
		fn create_token(origin) {
			let sender = ensure_signed(origin)?;

			Self::do_create_token(&sender)?;
		}
	}
}


decl_event!(
	pub enum Event<T> where
		AccountId = <T as system::Trait>::AccountId,
		TokenId = <T as Trait>::TokenId,
	{
		Approval(AccountId,  Option<AccountId>, TokenId),

		ApprovalForAll(AccountId, AccountId, bool),

		Transfer(AccountId, AccountId, TokenId),
	}
);


impl<T: Trait> Module<T> {

	fn do_appove(sender: &T::AccountId, to: &Option<T::AccountId>, token_id: T::TokenId) -> Result {
		let owner = Self::owner_of(token_id);

		ensure!(sender == &owner || Self::is_approved_for_all((owner.clone(), sender.clone())), "You do not have access for this token");

		if let Some(t) = to {
			ensure!(&owner != t, "Can not approve to yourself");
			<TokenToApproval<T>>::insert(token_id, t.clone());
		} else {
			<TokenToApproval<T>>::remove(token_id);
		}


		Ok(())
	}

	fn do_appove_for_all(owner: &T::AccountId, to: &T::AccountId, approved: bool) {
		<OwnerToOperator<T>>::insert((owner.clone(), to.clone()), approved);
	}

	fn do_transfer(from: &T::AccountId, to: &T::AccountId, token_id: T::TokenId) -> Result {
		// update balance
		let from_balance = Self::balance_of(from);
        let to_balance = Self::balance_of(to);
		let new_from_balance = match from_balance.checked_sub(&1.into()) {
            Some (c) => c,
            None => return Err("from account balance sub error"),
        };
        let new_to_balance = match to_balance.checked_add(&1.into()) {
            Some(c) => c,
            None => return Err("to account balance add error"),
        };
        <OwnerCount<T>>::insert(from, new_from_balance);
        <OwnerCount<T>>::insert(to, new_to_balance);

		// token approve remove
		Self::approval_clear(token_id)?;

		// token transfer
		<OwnerToTokenList<T>>::remove(&from, token_id);
		<OwnerToTokenList<T>>::append(&to, token_id);  

		// update token -- owner
		<TokenToOwner<T>>::insert(token_id, to);
		Ok(())
	}

	fn approval_clear(token_id: T::TokenId) -> Result { 
		<TokenToApproval<T>>::remove(token_id);
        Ok(())
    }

	fn do_create_token(owner: &T::AccountId) -> Result {
		let token_id = Self::total_supply();
		if token_id == T::TokenId::max_value() {
			return Err("TokenId overflow");
		}
		let balance =Self::balance_of(owner);

		<TokenToOwner<T>>::insert(token_id, owner);
		<OwnerCount<T>>::insert(owner, balance + 1.into());
		<TotalSupply<T>>::put(token_id + 1.into());

		<OwnerToTokenList<T>>::append(owner, token_id); 

		Ok(()) 
	}
}

/// impl NFTCurrency Module
impl<T: Trait> NFTCurrency<T::AccountId> for Module<T> {

	type TokenId = T::TokenId;

	type Currency= T::Currency;

	fn symbol() -> Vec<u8> {
		Self::symbol()
	}

	fn name() -> Vec<u8> {
		Self::name()
	}

	fn token_uri(token_id: Self::TokenId) -> Vec<u8> {
		Self::token_uri(token_id)
	}

	fn owner_of(token_id: Self::TokenId) -> T::AccountId {
		Self::owner_of(token_id)
	}

	fn balance_of(account: &T::AccountId) -> Self::TokenId {
		Self::balance_of(account)
	}
	
	fn get_approved(token_id: Self::TokenId) -> Option<T::AccountId> {
		Self::get_approved(token_id)
	}

	fn is_approved_for_all(account_approved: (T::AccountId, T::AccountId)) -> bool {
		Self::is_approved_for_all(account_approved)
	}

	fn total_supply() -> Self::TokenId {
		Self::total_supply()
	}

	fn owner_to_token(account_token: (T::AccountId, Option<Self::TokenId>)) -> Option<LinkedItem<Self::TokenId>> {
		Self::owner_to_token(account_token)
	}

	fn approve(
		who: &T::AccountId, 
		to:  &Option<T::AccountId>, 
		token_id: Self::TokenId
	) -> Result {
		Self::do_appove(who, to, token_id)
	}

	fn set_approval_for_all(
		who: &T::AccountId, 
		to: &T::AccountId, 
		approved: bool
	) -> Result {
		ensure!(who != to, "Can not approve to yourself");
		Self::do_appove_for_all(who, to, approved);
		Ok(())
	}

	// transfer
	fn transfer_from(
		who: &T::AccountId, 
		from: &T::AccountId, 
		to: &T::AccountId, 
		token_id: Self::TokenId
	) -> Result {
		let token_owner = Self::owner_of(token_id);
		ensure!(from == &token_owner, "not token owner");
		let approved_account = Self::get_approved(token_id);
		let is_owner = who == &token_owner;
		let is_approved = approved_account.is_some() && &approved_account.unwrap() == who;
		let is_approved_for_all = Self::is_approved_for_all((from.clone(), who.clone()));
		
		ensure!(is_owner || is_approved || is_approved_for_all, "You do not own this token auth");

		// do transfer
		Self::do_transfer(&token_owner, to, token_id)
	}

	// safe transfer
	fn safe_transfer_from(
		who: &T::AccountId, 
		from: &T::AccountId, 
		to: &T::AccountId, 
		token_id: Self::TokenId
	) -> Result {
		let balances = T::Currency::free_balance(&to);
		ensure!(!balances.is_zero(), "to account balances is zero");
		// transfer

		//the same with transfer_from
		let token_owner = Self::owner_of(token_id);
		ensure!(from == &token_owner, "not token owner");
		let approved_account = Self::get_approved(token_id);
		let is_approved_or_owner = who == &token_owner || Some(who.clone()) == approved_account || Self::is_approved_for_all((from.clone(), who.clone()));
		ensure!(is_approved_or_owner, "You do not own this token auth");

		// do transfer
		Self::do_transfer(&token_owner, to, token_id)
	}
}

/// tests for this module
#[cfg(test)]
mod tests {
	use super::*;

	use runtime_io::with_externalities;
	use primitives::{H256, Blake2Hasher};
	use support::{impl_outer_origin, assert_ok, assert_err, parameter_types};
	use sr_primitives::{traits::{BlakeTwo256, IdentityLookup}, testing::Header};
	use sr_primitives::weights::Weight;
	use sr_primitives::Perbill;

	impl_outer_origin! {
		pub enum Origin for Test {}
	}

	// For testing the module, we construct most of a mock runtime. This means
	// first constructing a configuration type (`Test`) which `impl`s each of the
	// configuration traits of modules we want to use.
	#[derive(Clone, Eq, PartialEq, Debug)]
	pub struct Test;
	parameter_types! {
		pub const BlockHashCount: u64 = 250;
		pub const MaximumBlockWeight: Weight = 1024;
		pub const MaximumBlockLength: u32 = 2 * 1024;
		pub const AvailableBlockRatio: Perbill = Perbill::from_percent(75);

		pub const ExistentialDeposit: u64 = 0;
        pub const TransferFee: u64 = 0;
        pub const CreationFee: u64 = 0;
        pub const TransactionBaseFee: u64 = 0;
        pub const TransactionByteFee: u64 = 0;
	}
	impl system::Trait for Test {
		type Origin = Origin;
		type Call = ();
		type Index = u64;
		type BlockNumber = u64;
		type Hash = H256;
		type Hashing = BlakeTwo256;
		type AccountId = u64;
		type Lookup = IdentityLookup<Self::AccountId>;
		type Header = Header;
		type WeightMultiplierUpdate = ();
		type Event = ();
		type BlockHashCount = BlockHashCount;
		type MaximumBlockWeight = MaximumBlockWeight;
		type MaximumBlockLength = MaximumBlockLength;
		type AvailableBlockRatio = AvailableBlockRatio;
		type Version = ();
	}
	impl balances::Trait for Test {
        type Balance = u64;
        type OnFreeBalanceZero = ();
        type OnNewAccount = ();
        type Event = ();
        type TransactionPayment = ();
        type TransferPayment = ();
        type DustRemoval = ();
        type ExistentialDeposit = ExistentialDeposit;
        type TransferFee = TransferFee;
        type CreationFee = CreationFee;
        type TransactionBaseFee = TransactionBaseFee;
        type TransactionByteFee = TransactionByteFee;
        type WeightToFee = ();
    }

	impl Trait for Test {
		type TokenId = u32;
		type Event = ();
		type Currency = balances::Module<Test>;
	}
	type NftModule = Module<Test>;
	type OwnerToTokenTest = OwnerToToken<Test>;
	type Balances = balances::Module<Test>;

    fn new_test_ext() -> runtime_io::TestExternalities<Blake2Hasher> {
        let mut t = system::GenesisConfig::default().build_storage::<Test>().unwrap();
        balances::GenesisConfig::<Test> {
            balances: vec![(1, 10), (2, 20), (3, 30), (4, 40), (5, 50), (6, 60), (7, 10), (8, 10), (9, 10), (10, 20)],
            vesting: vec![],
        }.assimilate_storage(&mut t).unwrap();
        t.into()
    }


	#[test]
	fn should_create_token() {
		with_externalities(&mut new_test_ext(), || {
			let owner = 1;
			let origin = Origin::signed(owner);
			
			let old_total_supply = NftModule::total_supply();
			let old_balance = NftModule::balance_of(owner);

			assert_eq!(OwnerToTokenTest::get(&(owner, Some(0))), None);

			assert_ok!(NftModule::create_token(origin.clone()));

			let new_total_supply = NftModule::total_supply();
			let new_balance = NftModule::balance_of(owner);

			assert_eq!(old_total_supply + 1, new_total_supply);
			assert_eq!(old_balance + 1, new_balance);
			assert_eq!(NftModule::owner_of(old_total_supply), owner);

			assert_eq!(OwnerToTokenTest::get(&(owner, None)), Some(TokenLinkedItem::<Test> {
				prev: Some(0),
				next: Some(0),
			}));

			assert_eq!(OwnerToTokenTest::get(&(owner, Some(0))), Some(TokenLinkedItem::<Test> {
				prev: None,
				next: None,
			}));
		});
	}

	#[test]
	fn should_approve() {
		with_externalities(&mut new_test_ext(), || {
			let owner = 1;
			let origin = Origin::signed(owner);
			let to = 2;
			let token_id = 0;

			assert_err!(NftModule::approve(origin.clone(), Some(to), token_id), "You do not have access for this token");
			assert_eq!(NftModule::get_approved(token_id), None);

			<TokenToOwner<Test>>::insert(token_id, owner);
			<OwnerCount<Test>>::insert(owner, 1);

			assert_err!(NftModule::approve(origin.clone(), Some(owner), token_id), "Can not approve to yourself");
			assert_ok!(NftModule::approve(origin.clone(), Some(to), token_id));
			assert_eq!(NftModule::get_approved(token_id).unwrap(), to);

			// should remove approve with appove to None
			assert_ok!(NftModule::approve(origin, None, token_id));
			assert_eq!(NftModule::get_approved(token_id), None);
		});
	}

	#[test]
	fn should_set_approval_for_all() {
		with_externalities(&mut new_test_ext(), || {
			let owner = 1;
			let origin = Origin::signed(owner);
			let to = 2;
			let approved = true;

			assert_err!(NftModule::set_approval_for_all(origin.clone(), owner, approved), "Can not approve to yourself");
			assert_ok!(NftModule::set_approval_for_all(origin, to, approved));
			assert_eq!(NftModule::is_approved_for_all((owner, to)), approved);
		});
	}



	#[test]
	fn should_transfer_from() {
		with_externalities(&mut new_test_ext(), || {
			{
				let from1 = 1;
				let to1 = 2;
				let token_id1 = 1;

				OwnerToTokenList::<Test>::append(&from1, token_id1);
				<TokenToOwner<Test>>::insert(token_id1, from1);
				<OwnerCount<Test>>::insert(from1, 1);

				let old_from_balance = NftModule::balance_of(from1);
				let old_to_balance = NftModule::balance_of(to1);

				assert_ok!(NftModule::transfer_from(Origin::signed(from1), from1, to1, token_id1));

				assert_eq!(NftModule::owner_of(token_id1), to1);

				let new_from_balance = NftModule::balance_of(from1);
				let new_to_balance = NftModule::balance_of(to1);

				assert_eq!(old_from_balance - 1, new_from_balance);
				assert_eq!(old_to_balance + 1, new_to_balance);
			}

			{
				let from2 = 3;
				let to2 = 4;
				let token_id2 = 2;
				let token_approve_account = 5;

				OwnerToTokenList::<Test>::append(&from2, token_id2);
				<TokenToOwner<Test>>::insert(token_id2, from2);
				<OwnerCount<Test>>::insert(from2, 1);
				<TokenToApproval<Test>>::insert(token_id2, token_approve_account);

				let old_from_balance = NftModule::balance_of(from2);
				let old_to_balance = NftModule::balance_of(to2);

				assert_ok!(NftModule::transfer_from(Origin::signed(token_approve_account), from2, to2, token_id2));

				assert_eq!(NftModule::owner_of(token_id2), to2);

				let new_from_balance = NftModule::balance_of(from2);
				let new_to_balance = NftModule::balance_of(to2);

				assert_eq!(old_from_balance - 1, new_from_balance);
				assert_eq!(old_to_balance + 1, new_to_balance);
			}

			{
				let from3 = 6;
				let to3 = 7;
				let token_id3 = 3;
				let account_approve_account = 8;
				OwnerToTokenList::<Test>::append(&from3, token_id3);
				<TokenToOwner<Test>>::insert(token_id3, from3);
				<OwnerCount<Test>>::insert(from3, 1);

				let old_from_balance = NftModule::balance_of(from3);
				let old_to_balance = NftModule::balance_of(to3);

				<OwnerToOperator<Test>>::insert((from3, account_approve_account), true);
				assert_ok!(NftModule::transfer_from(Origin::signed(account_approve_account), from3, to3, token_id3));

				assert_eq!(NftModule::owner_of(token_id3), to3);

				let new_from_balance = NftModule::balance_of(from3);
				let new_to_balance = NftModule::balance_of(to3);

				assert_eq!(old_from_balance - 1, new_from_balance);
				assert_eq!(old_to_balance + 1, new_to_balance);
			}
		});
	}

	#[test]
	fn should_safe_transfer_from() {
		with_externalities(&mut new_test_ext(), || {
			let from = 9;
			let origin = Origin::signed(from);
			let to = 10;
			let token_id = 4;

			OwnerToTokenList::<Test>::append(&from, token_id);
			<TokenToOwner<Test>>::insert(token_id, from);
			<OwnerCount<Test>>::insert(from, 1);
			assert_eq!(Balances::free_balance(to), 20);


			let old_from_balance = NftModule::balance_of(from);
			let old_to_balance = NftModule::balance_of(to);

			assert_ok!(NftModule::safe_transfer_from(origin, from, to, token_id));

			assert_eq!(NftModule::owner_of(token_id), to);

			let new_from_balance = NftModule::balance_of(from);
			let new_to_balance = NftModule::balance_of(to);

			assert_eq!(old_from_balance - 1, new_from_balance);
			assert_eq!(old_to_balance + 1, new_to_balance);
		});
	}

	#[test]
	fn owned_to_token_can_append_values() {
		with_externalities(&mut new_test_ext(), || {
			OwnerToTokenList::<Test>::append(&0, 1);

			assert_eq!(OwnerToTokenTest::get(&(0, None)), Some(TokenLinkedItem::<Test> {
				prev: Some(1),
				next: Some(1),
			}));

			assert_eq!(OwnerToTokenTest::get(&(0, Some(1))), Some(TokenLinkedItem::<Test> {
				prev: None,
				next: None,
			}));

			OwnerToTokenList::<Test>::append(&0, 2);

			assert_eq!(OwnerToTokenTest::get(&(0, None)), Some(TokenLinkedItem::<Test> {
				prev: Some(2),
				next: Some(1),
			}));

			assert_eq!(OwnerToTokenTest::get(&(0, Some(1))), Some(TokenLinkedItem::<Test> {
				prev: None,
				next: Some(2),
			}));

			assert_eq!(OwnerToTokenTest::get(&(0, Some(2))), Some(TokenLinkedItem::<Test> {
				prev: Some(1),
				next: None,
			}));

			OwnerToTokenList::<Test>::append(&0, 3);

			assert_eq!(OwnerToTokenTest::get(&(0, None)), Some(TokenLinkedItem::<Test> {
				prev: Some(3),
				next: Some(1),
			}));

			assert_eq!(OwnerToTokenTest::get(&(0, Some(1))), Some(TokenLinkedItem::<Test> {
				prev: None,
				next: Some(2),
			}));

			assert_eq!(OwnerToTokenTest::get(&(0, Some(2))), Some(TokenLinkedItem::<Test> {
				prev: Some(1),
				next: Some(3),
			}));

			assert_eq!(OwnerToTokenTest::get(&(0, Some(3))), Some(TokenLinkedItem::<Test> {
				prev: Some(2),
				next: None,
			}));
		});
	}

	#[test]
	fn owned_to_token_can_remove_values() {
		with_externalities(&mut new_test_ext(), || {
			OwnerToTokenList::<Test>::append(&0, 1);
			OwnerToTokenList::<Test>::append(&0, 2);
			OwnerToTokenList::<Test>::append(&0, 3);

			OwnerToTokenList::<Test>::remove(&0, 2);

			assert_eq!(OwnerToTokenTest::get(&(0, None)), Some(TokenLinkedItem::<Test> {
				prev: Some(3),
				next: Some(1),
			}));

			assert_eq!(OwnerToTokenTest::get(&(0, Some(1))), Some(TokenLinkedItem::<Test> {
				prev: None,
				next: Some(3),
			}));

			assert_eq!(OwnerToTokenTest::get(&(0, Some(2))), None);

			assert_eq!(OwnerToTokenTest::get(&(0, Some(3))), Some(TokenLinkedItem::<Test> {
				prev: Some(1),
				next: None,
			}));

			OwnerToTokenList::<Test>::remove(&0, 1);

			assert_eq!(OwnerToTokenTest::get(&(0, None)), Some(TokenLinkedItem::<Test> {
				prev: Some(3),
				next: Some(3),
			}));

			assert_eq!(OwnerToTokenTest::get(&(0, Some(1))), None);

			assert_eq!(OwnerToTokenTest::get(&(0, Some(2))), None);

			assert_eq!(OwnerToTokenTest::get(&(0, Some(3))), Some(TokenLinkedItem::<Test> {
				prev: None,
				next: None,
			}));

			OwnerToTokenList::<Test>::remove(&0, 3);

			assert_eq!(OwnerToTokenTest::get(&(0, None)), Some(TokenLinkedItem::<Test> {
				prev: None,
				next: None,
			}));

			assert_eq!(OwnerToTokenTest::get(&(0, Some(1))), None);

			assert_eq!(OwnerToTokenTest::get(&(0, Some(2))), None);

			assert_eq!(OwnerToTokenTest::get(&(0, Some(2))), None);
		});
	}

}
