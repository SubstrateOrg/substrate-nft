/// A runtime module template with necessary imports

/// Feel free to remove or edit this file as needed.
/// If you change the name of this file, make sure to update its references in runtime/src/lib.rs
/// If you remove this file, you can remove those references


/// For more guidance on Substrate modules, see the example module
/// https://github.com/paritytech/substrate/blob/master/srml/example/src/lib.rs

use support::{decl_module, decl_storage, decl_event, Parameter, StorageMap, StorageValue, 
	dispatch::Result, ensure, traits::Currency
};
use sr_primitives::traits::{SimpleArithmetic, Bounded, Member, Zero};
use system::ensure_signed;
use rstd::vec::Vec;
use crate::linked_item::{LinkedList, LinkedItem};

/// The module's configuration trait.
pub trait Trait: system::Trait {
	// TODO: Add other types and constants required configure this module.

	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
	
	type TokenId: Parameter + Member + SimpleArithmetic + Bounded + Default + Copy;

	type Currency: Currency<Self::AccountId>;
}

type TokenLinkedItem<T> = LinkedItem<<T as Trait>::TokenId>;
type OwnerToTokenList<T> = LinkedList<OwnerToToken<T>, <T as system::Trait>::AccountId, <T as Trait>::TokenId>;

// This module's storage items.
decl_storage! {
	trait Store for Module<T: Trait> as Nft {
		
		//#region ERC-721 metadata extension

		/// 代币符号，默认NFT，Bytes存储
		pub Symbol get(symbol)  config(): Vec<u8>;

		/// 代币名称，默认Non-Fungible Token，Bytes存储
		pub Name get(name)  config(): Vec<u8>;

		/// 代币元数据uri
		pub TokenURI get(token_uri): map T::TokenId => Vec<u8>;

		//#endregion


		//#region ERC-721 compliant contract

		/// TokenId到账户的Map，用于保存、查找和修改Token持有人
		pub TokenToOwner get(owner_of): map T::TokenId => T::AccountId;

		/// 账户到余额的Map，用于保存和查询账户持有Token数量
		//pub OwnerCount get(balance_of): map T::AccountId => T::TokenId;  // how to transfer int type to T::TokenId type? eg: String, Hash
		pub OwnerCount get(balance_of): map T::AccountId => u64;

		/// Token到授权账户的Map，用于保存和查询对单个Token的授权
		pub TokenToApproval get(get_approved): map T::TokenId => Option<T::AccountId>;

		/// 一个账户对另一个账户是否授权的Map，用于保存和查询对一个账户对另一个账户的授权
		pub OwnerToOperator get(is_approved_for_all): map (T::AccountId, T::AccountId) => bool;

		//#endregion ERC721标准


		//#region ERC-721 enumeration extension
		/// Token总发行量
		pub TotalSupply get(total_supply): T::TokenId;

		/// TokenId即TokenIndex，无需获取
		//pub TokenByIndex get(token_by_index): T::TokenId => T::TokenId;

		/// TokenId即TokenIndex，无需获取
		//pub TokenOfOwnerByIndex get(token_of_owner_by_index): map (T::AccountId, T::TokenId) => T::TokenId;
		//#endregion


		//#region 其他索引
		/// 持有人和持有Token到链表的Map，用于查询持有人下的所有Token，并支持O(1)复杂度的修改持有人
		pub OwnerToToken get(owner_to_token): map (T::AccountId, Option<T::TokenId>) => Option<TokenLinkedItem<T>>;

		//endregion

		//#region Just for Demo
		Something get(something): Option<u32>;
		//#endregion
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

		// Just a dummy entry point.
		// function that can be called by the external world as an extrinsics call
		// takes a parameter of the type `AccountId`, stores it and emits an event
		pub fn do_something(origin, something: u32) -> Result {
			// TODO: You only need this if you want to check it was signed.
			let who = ensure_signed(origin)?;

			// TODO: Code to execute when something calls this.
			// For example: the following line stores the passed in u32 in the storage
			Something::put(something);

			// here we are raising the Something event
			Self::deposit_event(RawEvent::SomethingStored(something, who));
			Ok(())
		}

		// transfer
		fn transfer_from(origin, from: T::AccountId, to: T::AccountId, token_id: T::TokenId) -> Result {
			let sender = ensure_signed(origin)?;
			let token_owner = Self::owner_of(token_id);
			let approved_account = Self::get_approved(token_id);
			let is_approved_or_owner = from == token_owner || from == approved_account.unwrap() || Self::is_approved_for_all((sender.clone(), from.clone()));
			ensure!(is_approved_or_owner, "You do not own this token auth");

			// do transfer
			Self::do_transfer(&from, &to, token_id)?;
			Self::deposit_event(RawEvent::Transfer(sender, to, token_id));
			Ok(())
		}

		// safe transfer
		fn safe_transfer_from(origin, from: T::AccountId, to: T::AccountId, token_id: T::TokenId) -> Result {
			// check to account balance is_zero
			let balances = T::Currency::free_balance(&to);
            ensure!(!balances.is_zero(), "to account balances is zero");
			// transfer
			Self::transfer_from(origin, from, to, token_id)?;
			Ok(())
		}
	}
}

decl_event!(
	pub enum Event<T> where 
		AccountId = <T as system::Trait>::AccountId,
		TokenId = <T as Trait>::TokenId,
	{
		// Just a dummy event.
		// Event `Something` is declared with a parameter of the type `u32` and `AccountId`
		// To emit this event, we call the deposit funtion, from our runtime funtions
		SomethingStored(u32, AccountId),

		Transfer(AccountId, AccountId, TokenId),
	}
);

impl<T: Trait> Module<T> {
	fn do_transfer(from: &T::AccountId, to: &T::AccountId, token_id: T::TokenId) -> Result {
		// update balance
		let from_balance = Self::balance_of(from);
        let to_balance = Self::balance_of(to);
		let new_from_balance = match from_balance.checked_sub(1) {
            Some (c) => c,
            None => return Err("from account balance sub error"),
        };
        let new_to_balance = match to_balance.checked_add(1) {
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
}

/// tests for this module
#[cfg(test)]
mod tests {
	use super::*;

	use runtime_io::with_externalities;
	use primitives::{H256, Blake2Hasher};
	use support::{impl_outer_origin, parameter_types, assert_ok};
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
	impl Trait for Test {
		type TokenId = u32;
		type Event = ();
	}

	type TemplateModule = Module<Test>;
	type OwnerToTokenTest = OwnerToToken<Test>;

	// This function basically just builds a genesis storage key/value store according to
	// our desired mockup.
	fn new_test_ext() -> runtime_io::TestExternalities<Blake2Hasher> {
		system::GenesisConfig::default().build_storage::<Test>().unwrap().into()
	}

	#[test]
	fn owne_to_token_can_append_values() {
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
	fn owne_to_token_can_remove_values() {
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

	#[test]
	fn owne_to_token_can_transfer_from() {
		with_externalities(&mut new_test_ext(), || {
			let from = 1;
			let to = 2;
			let token_id = 0;

			<TokenToOwner<Test>>::insert(token_id, from);
			assert_ok!(TemplateModule::transfer_from(Origin::signed(from), from, to, token_id));

			let token_approve_account = 3;
			<TokenToApproval<Test>>::insert(token_id, token_approve_account);
			assert_ok!(TemplateModule::transfer_from(Origin::signed(from), token_approve_account, to, token_id));

			let account_approve_account = 4;
			<OwnerToOperator<Test>>::insert((from, account_approve_account), true);
			assert_ok!(TemplateModule::transfer_from(Origin::signed(from), account_approve_account, to, token_id));
		});
	}

	#[test]
	fn owne_to_token_can_safe_transfer_from() {
		with_externalities(&mut new_test_ext(), || {
			let from = 1;
			let origin = Origin::signed(from);
			let to = 2;
			let token_id = 0;

			<TokenToOwner<Test>>::insert(token_id, from);
			assert_ok!(TemplateModule::safe_transfer_from(origin, from, to, token_id));
		});
	}
}

